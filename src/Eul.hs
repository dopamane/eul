module Eul where

import Clash.Prelude

import Control.Lens ( makeLenses, use, to, (^.), (.=), (+=) )
import Control.Monad.State

import Rstn ( rstn )
import Spi  ( spiWorkerTx )

type Addr    = Index 8
type Reg     = BitVector 32
type RegBank = Vec 8 Reg
type Imm     = BitVector 16

data Instr
  = Add Addr Addr Addr
  | Sub Addr Addr Addr
  | Mul Addr Addr Addr
  | PutH Addr Imm
  | PutL Addr Imm
  | Beq Addr Addr Addr
  | Get Addr
  | Nop

-- Add  => 0b000 => 0
-- Sub  => 0b001 => 1
-- Mul  => 0b010 => 2
-- PutH => 0b011 => 3
-- PutL => 0b100 => 4
-- Beq  => 0b101 => 5
-- Get  => 0b110 => 6
-- Nop  => 0b111 => 7

data Stage = Fetch | Execute | Write

data Eul n = Eul
  { _instr :: Instr
  , _stage :: Stage
  , _regs  :: RegBank
  , _pc    :: Index n
  }
makeLenses ''Eul

{-# ANN topEntity
  (Synthesize
    { t_name = "EUL"
    , t_inputs = [ PortName "clk"
                 , PortName "SCK"
                 , PortName "SS"
                 ]
    , t_output = PortName "MISO"
    })#-}
topEntity
  :: Clock System 'Source -- clk
  -> Signal System Bit    -- sck
  -> Signal System Bool   -- ss
  -> Signal System Bit    -- miso
topEntity clk = withClockReset clk rst (eul prog)
  where
    rst = rstn d16 clk
{-# NOINLINE topEntity #-}

eul
  :: HiddenClockReset dom gated sync
  => (KnownNat n, 1 <= n)
  => Vec n Instr
  -> Signal dom Bit
  -> Signal dom Bool
  -> Signal dom Bit
eul romContent sck ss = miso
  where
    (miso, ack) = spiWorkerTx txLd sck ss
    txLd = moore (eulT romContent) writeBack initial ack
    initial = Eul Nop Fetch (repeat 0) 0

writeBack :: Eul n -> Maybe (BitVector 32)
writeBack Eul{_instr=(Get i), _stage=Write, _regs=r} = Just $ r !! i
writeBack _ = Nothing

eulT :: (KnownNat n, 1 <= n) => Vec n Instr -> Eul n -> Bool -> Eul n
eulT romContent s ack = flip execState s $ case s^.stage of
  Fetch   -> s^.pc.to (fetch romContent)
  Execute -> execute
  Write   -> when ack $ stage .= Fetch

fetch :: KnownNat n => Vec n Instr -> Index n -> State (Eul n) ()
fetch romContent instrAddr = do
  instr .= asyncRom romContent instrAddr
  stage .= Execute

execute :: (KnownNat n, 1 <= n) => State (Eul n) ()
execute = do
  r     <- use regs
  ins   <- use instr
  curPC <- use pc
  regs .= case ins of
    Add  a b c -> writeReg r c $ (r !! a) + (r !! b)
    Sub  a b c -> writeReg r c $ (r !! a) - (r !! b)
    Mul  a b c -> writeReg r c $ (r !! a) * (r !! b)
    PutH a i   -> writeReg r a (i ++# getLower (r !! a))
    PutL a i   -> writeReg r a (getHigher (r !! a) ++# i)
    _          -> r
  stage .= case ins of
    Get _ -> Write
    _     -> Fetch
  case ins of
    Beq a b pcRegAddr | (r !! a) == (r !! b) -> pc .= unpack (resize $ r !! pcRegAddr)
    _  -> unless (curPC == maxBound) $ pc += 1
  where
    getHigher = slice d31 d16
    getLower  = slice d15 d0

writeReg
  :: RegBank
  -> Addr
  -> Reg
  -> RegBank
writeReg r i d = replace i d r

--   31   28   25   22   19    15               0
-- 0b[***][***][***][***][****][****************]
-- opcode|adr1|adr2|adr3|unused|immediate

decode :: BitVector 32 -> Maybe Instr
decode bs = case slice d31 d29 bs of
  0 -> Just $ Add  addr1 addr2 addr3
  1 -> Just $ Sub  addr1 addr2 addr3
  2 -> Just $ Mul  addr1 addr2 addr3
  3 -> Just $ PutH addr1 imm
  4 -> Just $ PutL addr1 imm
  5 -> Just $ Beq  addr1 addr2 addr3
  6 -> Just $ Get  addr1
  7 -> Just Nop
  _ -> Nothing
  where
    addr1 = unpack $ slice d28 d26 bs
    addr2 = unpack $ slice d25 d23 bs
    addr3 = unpack $ slice d22 d20 bs
    imm   = slice d15 d0 bs

prog :: Vec 5 Instr
prog =  PutL 0 5
     :> PutL 1 7
     :> Add  0 1 2
     :> Get  2
     :> Nop
     :> Nil

{-
sample program using narco:
PutL 0 5   = 0b100 000 00 0x00 0x00 0x05      = 0x80 0x00 0x00 0x05
PutL 1 7   = 0b100 001 00 0x00 0x00 0x07      = 0x84 0x00 0x00 0x07
Add  0 1 2 = 0b000 000 001 010 **** 0x00 0x00 = 0x00 0xa0 0x00 0x00
Get  2     = 0b101 010 00 0x00 0x00 0x00      = 0xa8 0x00 0x00 0x00

narco -WB 0x80 0x00 0x00 0x05
narco -WB 0x84 0x00 0x00 0x07
narco -WB 0x00 0xa0 0x00 0x00
narco -WB 0xa8 0x00 0x00 0x00
narco -RB 4 = 0x00 0x00 0x00 0x0c
-}
