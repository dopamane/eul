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
  | Mov Addr Addr
  | Get Addr
  | Nop

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
    Mov a b    -> writeReg r b $ r !! a
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

prog :: Vec 5 Instr
prog =  PutL 0 5
     :> PutL 1 7
     :> Add  0 1 2
     :> Get  2
     :> Nop
     :> Nil

fib :: Vec 14 Instr
fib =  PutL 0 10 -- nth  fibonacci number 10 -> r0
    :> PutL 1 0  -- prev prev              0 -> r1
    :> PutL 2 1  -- prev                   1 -> r2
    :> PutL 3 2  -- i                      2 -> r3
    :> PutL 4 1  -- increment              1 -> r4
    :> PutL 5 6  -- loop begin addr        6 -> r5
    :> Add 1 2 6 -- prev prev + prev r1 + r2 -> r6 LOOP BEGIN
    :> Mov 2 1   -- prev -> prev prev     r2 -> r1
    :> Mov 6 2   -- fib -> prev           r6 -> r2
    :> Add 3 4 7 -- i + 1            r3 + r4 -> r7
    :> Mov 7 3   --                       r7 -> r3
    :> Beq 3 0 6 -- goto LOOP BEGIN if i == n
    :> Get 2     -- spi write
    :> Nop       -- END
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
