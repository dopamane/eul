{-# LANGUAGE LambdaCase #-}
module Eul where

import Clash.Prelude

import Control.Lens ( makeLenses, use, (^.), (.=), (%=) )
import Control.Monad.State
import Data.Maybe ( isJust )
import Data.Bool ( bool )

import Rstn ( rstn )
import Spi  ( spiWorkerTx )

type Addr n    = Index n
type Reg       = BitVector 32
type RegBank n = Vec n Reg
type Imm       = BitVector 16
type PC p      = Unsigned p
type Ram m     = Unsigned m

data Instr n m
  = Add   (Addr n) (Addr n) (Addr n)
  | Sub   (Addr n) (Addr n) (Addr n)
  | Mul   (Addr n) (Addr n) (Addr n)
  | ImmH  (Addr n) Imm
  | ImmL  (Addr n) Imm
  | Bne   (Addr n) (Addr n) (Addr n)
  | Mov   (Addr n) (Addr n)
  | Get   (Addr n)
  | Load  (Addr n) (Ram m)
  | Store (Addr n) (Ram m)
  | Nop

data Eul n m = Eul
  { _exir  :: Instr n m
  , _memir :: Instr n m
  , _regs  :: RegBank n
  , _pc    :: PC m
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
topEntity clk = withClockReset clk rst (eul ramContent)
  where
    rst = rstn d16 clk
    ramContent = map encode fib ++ repeat 0
{-# NOINLINE topEntity #-}

eul
  :: HiddenClockReset dom gated sync
  => Vec (2^10) Reg
  -> Signal dom Bit
  -> Signal dom Bool
  -> Signal dom Bit
eul ramContent sck ss = miso
  where
    (miso, ack) = spiWorkerTx txLd sck ss
    (txLd, pcAddr, rdAddr, wrM) = mooreB eulT eulO initial (ack, pcValue, rdValue)
    initial = Eul Nop Nop (repeat 0) 0
    rdValue = readNew (blockRamPow2 ramContent) rdAddr wrM
    pcValue = readNew (blockRamPow2 ramContent) pcAddr wrM

eulO
  :: (KnownNat n, KnownNat m)
  => Eul n m
  -> (Maybe Reg, PC m, Ram m, Maybe (Ram m, Reg))
eulO s = (txLd, s^.pc, rdAddr, wrM)
  where
    txLd = case s^.exir of
      Get i -> Just $ (s^.regs) !! i
      _ -> Nothing
    rdAddr = case s^.exir of
      Load _ m -> m
      _ -> 0
    wrM = case s^.memir of
      Store a m -> Just (m, (s^.regs) !! a)
      _ -> Nothing

eulT :: Eul 8 10 -> (Bool, Reg, Reg) -> Eul 8 10
eulT s (ack, pcValue, rdValue) = flip execState s $ do
  (memBranch, ldReg) <- memory rdValue
  (exBranch, stall) <- execute ack ldReg
  fetch stall exBranch memBranch $ decode pcValue

fetch ::Bool -> Maybe (PC 10) -> Bool -> Instr 8 10 -> State (Eul 8 10) ()
fetch stall exBranch memBranch pcValue = unless stall $ do
  pc %= updatePC exBranch
  exir .= bool pcValue Nop (isJust exBranch || memBranch)
  where
    updatePC (Just b) = const b
    updatePC _ = (+1)

execute
   :: (KnownNat n, KnownNat m)
   => Bool
   -> Maybe (Addr n, Reg)
   -> State (Eul n m) (Maybe (PC m), Bool)
execute ack ldReg = do
  instr <- use exir
  regs %= case ldReg of
    Just (a, i) -> replace a i
    _ -> id
  r <- use regs
  regs %= case instr of
    Add    a b c -> replace c $ (r !! a) + (r !! b)
    Sub    a b c -> replace c $ (r !! a) - (r !! b)
    Mul    a b c -> replace c $ (r !! a) * (r !! b)
    ImmH a i   -> replace a $ i ++# getLower (r !! a)
    ImmL a i   -> replace a $ getHigher (r !! a) ++# i
    Mov    a b   -> replace b $ r !! a
    _  -> id
  memir .= instr
  return $ case instr of
    Bne a b pcRegAddr | (r !! a) /= (r !! b) -> (Just $ unpack $ resize $ r !! pcRegAddr, False)
    Get _ | not ack -> (Nothing, True)
    _  -> (Nothing, False)
  where
    getHigher = slice d31 d16
    getLower  = slice d15 d0

memory :: Reg -> State (Eul n m) (Bool, Maybe (Addr n, Reg))
memory ramValue = do
  ir <- use memir
  return $ case ir of
    Load a _ -> (False, Just (a, ramValue))
    Bne{} -> (True, Nothing)
    _ -> (False, Nothing)

{- OPCODE :: BitVector 4
Add    => 0
Sub    => 1
Mul    => 2
LoadIH => 3
LoadIL => 4
Bne    => 5
Mov    => 6
Get    => 7
Load   => 8
Store  => 9
Nop    => 10-15
-}

--   31    27   24   21   18   15               0
-- 0b[****][***][***][***][***][****************]
--  opcode|adr1|adr2|adr3|unusd|immediate OR mem[9:0]

decode :: Reg -> Instr 8 10
decode bs = case slice d31 d28 bs of
  0 -> Add    addr1 addr2 addr3
  1 -> Sub    addr1 addr2 addr3
  2 -> Mul    addr1 addr2 addr3
  3 -> ImmH addr1 imm
  4 -> ImmL addr1 imm
  5 -> Bne    addr1 addr2 addr3
  6 -> Mov    addr1 addr2
  7 -> Get    addr1
  8 -> Load   addr1 (unpack mem)
  9 -> Store  addr1 (unpack mem)
  _ -> Nop
  where
    addr1 = unpack $ slice d27 d25 bs
    addr2 = unpack $ slice d24 d22 bs
    addr3 = unpack $ slice d21 d19 bs
    imm   = slice d15 d0 bs
    mem   = slice d9  d0 bs

encode :: Instr 8 10 -> Reg
encode = \case
  Add    addr1 addr2 addr3 -> 0b0000 ++# pack addr1 ++# pack addr2 ++# pack addr3 ++# (0 :: BitVector 19)
  Sub    addr1 addr2 addr3 -> 0b0001 ++# pack addr1 ++# pack addr2 ++# pack addr3 ++# (0 :: BitVector 19)
  Mul    addr1 addr2 addr3 -> 0b0010 ++# pack addr1 ++# pack addr2 ++# pack addr3 ++# (0 :: BitVector 19)
  ImmH addr1 imm         -> 0b0011 ++# pack addr1 ++# (0 :: BitVector 9) ++# imm
  ImmL addr1 imm         -> 0b0100 ++# pack addr1 ++# (0 :: BitVector 9) ++# imm
  Bne    addr1 addr2 addr3 -> 0b0101 ++# pack addr1 ++# pack addr2 ++# pack addr3 ++# (0 :: BitVector 19)
  Mov    addr1 addr2       -> 0b0110 ++# pack addr1 ++# pack addr2 ++# (0 :: BitVector 22)
  Get    addr1             -> 0b0111 ++# pack addr1 ++# (0 :: BitVector 25)
  Load   addr1 imm         -> 0b1000 ++# pack addr1 ++# (0 :: BitVector 15) ++# pack imm
  Store  addr1 imm         -> 0b1001 ++# pack addr1 ++# (0 :: BitVector 15) ++# pack imm
  Nop                      -> 0b1111 ++# (0 :: BitVector 28)

prog :: Vec 4 (Instr 8 10)
prog =  ImmL 0 5
     :> ImmL 1 7
     :> Add  0 1 2
     :> Get  2
     :> Nil

fib :: Vec 13 (Instr 8 10)
fib =  ImmL 0 29 -- nth  fibonacci number 10 -> r0
    :> ImmL 1 0  -- prev prev              0 -> r1
    :> ImmL 2 1  -- prev                   1 -> r2
    :> ImmL 3 2  -- i                      2 -> r3
    :> ImmL 4 1  -- increment              1 -> r4
    :> ImmL 5 6  -- loop begin addr        7 -> r5
    :> Add 1 2 6 -- prev prev + prev r1 + r2 -> r6 LOOP BEGIN
    :> Mov 2 1   -- prev -> prev prev     r2 -> r1
    :> Mov 6 2   -- fib -> prev           r6 -> r2
    :> Add 3 4 7 -- i + 1            r3 + r4 -> r7
    :> Mov 7 3   --                       r7 -> r3
    :> Bne 3 0 5 -- goto LOOP BEGIN if i /= n
    :> Get 2     -- spi write
    :> Nil

ramTest :: Vec 12 (Instr 8 10)
ramTest =  ImmL 0 5
        :> Store 0 200
        :> ImmL 0 4
        :> Store 0 201
        :> Load 0 200
        :> Load 1 201
        :> Add 0 1 2
        :> Get 2
        :> Nil ++ jmpBegin

ramReadNew :: Vec 10 (Instr 8 10)
ramReadNew =  ImmL 0 5
           :> ImmL 1 3
           :> Add 0 1 2
           :> Store 2 200
           :> Load 0 200
           :> Get 0
           :> Nil ++ jmpBegin

jmpBegin :: Vec 4 (Instr 8 10)
jmpBegin =  ImmL 0 0
         :> ImmL 1 1
         :> ImmL 2 0
         :> Bne 0 1 2 -- Loop Back to Beginning
         :> Nil
