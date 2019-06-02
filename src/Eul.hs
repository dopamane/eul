{-# LANGUAGE LambdaCase #-}
module Eul where

import Clash.Prelude

import Control.Lens ( makeLenses, use, uses, (^.), (.=), (%=) )
import Control.Monad.State
import Data.Maybe ( isJust, isNothing, fromMaybe )
import Data.Bool ( bool )

import Rstn ( rstn )
import Spi  ( spiWorker )

type Addr n    = Unsigned n
type Reg       = BitVector 32
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
  | Put   (Addr n)
  | Load  (Addr n) (Ram m)
  | Store (Addr n) (Ram m)
  | Nop

data Eul n m = Eul
  { _exir     :: Instr n m
  , _memir    :: Instr n m
  , _regWrite :: Reg
  , _pc       :: PC m
  }
makeLenses ''Eul

{-# ANN topEntity
  (Synthesize
    { t_name = "EUL"
    , t_inputs = [ PortName "clk"
                 , PortName "SCK"
                 , PortName "SS"
                 , PortName "MOSI"
                 ]
    , t_output = PortName "MISO"
    })#-}
topEntity
  :: Clock System 'Source -- clk
  -> Signal System Bit    -- sck
  -> Signal System Bool   -- ss
  -> Signal System Bit    -- mosi
  -> Signal System Bit    -- miso
topEntity clk = withClockReset clk rst (eul ramContent)
  where
    rst = rstn d16 clk
    ramContent = map encode putTest ++ repeat 0
{-# NOINLINE topEntity #-}

eul
  :: HiddenClockReset dom gated sync
  => Vec (2^10) Reg
  -> Signal dom Bit
  -> Signal dom Bool
  -> Signal dom Bit
  -> Signal dom Bit
eul ramContent sck ss mosi = miso
  where
    (miso, ack, spiRx) = spiWorker txLd sck ss mosi
    (txLd, pcAddr, rdAddr, wrM, regAddr1, regAddr2, regAddr3, regWrM) = mealyB eulT initial (ack, pcValue, rdValue, spiRx, regRds)
    initial = Eul Nop Nop 0 0
    rdValue = readNew (blockRamPow2 ramContent) rdAddr wrM
    pcValue = readNew (blockRamPow2 ramContent) pcAddr wrM
    regRds = regBank regAddr1 regAddr2 regAddr3 regWrM

eulT
  :: Eul 3 10
  -> (Bool, Reg, Reg, Maybe Reg, (Reg, Reg, Reg))
  -> (Eul 3 10, (Maybe Reg, PC 10, Ram 10, Maybe (Ram 10, Reg), Addr 3, Addr 3, Addr 3, Maybe (Addr 3, Reg)))
eulT s i@(_, _, ramValue, _, (r1, _, _)) = (s', o)
  where
    s' = eulS s i
    o = eulO s ramValue r1

eulO
  :: (KnownNat n, KnownNat m)
  => Eul n m
  -> Reg
  -> Reg
  -> (Maybe Reg, PC m, Ram m, Maybe (Ram m, Reg), Addr n, Addr n, Addr n, Maybe (Addr n, Reg))
eulO s ramValue r1 = (txLd, s^.pc, rdAddr, wrM, regAddr1, regAddr2, regAddr3, regWrM)
  where
    txLd = case s^.exir of
      Get _ -> Just r1
      _ -> Nothing
    rdAddr = case s^.exir of
      Load _ m -> m
      _ -> 0
    wrM = case s^.memir of
      Store _ m -> Just (m, s^.regWrite)
      _ -> Nothing
    regAddr1 = case s^.exir of
      Add   a _ _ -> a
      Sub   a _ _ -> a
      Mul   a _ _ -> a
      Bne   a _ _ -> a
      Mov   a _   -> a
      Get   a     -> a
      Store a _   -> a
      _           -> 0
    regAddr2 = case s^.exir of
      Add _ b _ -> b
      Sub _ b _ -> b
      Mul _ b _ -> b
      Bne _ b _ -> b
      _         -> 0
    regAddr3 = case s^.exir of
      Bne _ _ c -> c
      _         -> 0
    regWrM = case s^.memir of
      Add _ _ c -> Just (c, s^.regWrite)
      Sub _ _ c -> Just (c, s^.regWrite)
      Mul _ _ c -> Just (c, s^.regWrite)
      ImmH a _  -> Just (a, s^.regWrite)
      ImmL a _  -> Just (a, s^.regWrite)
      Mov _ b   -> Just (b, s^.regWrite)
      Put a     -> Just (a, s^.regWrite)
      Load a _  -> Just (a, ramValue)
      _         -> Nothing

eulS
  :: Eul 3 10
  -> (Bool, Reg, Reg, Maybe Reg, (Reg, Reg, Reg))
  -> Eul 3 10
eulS s (ack, pcValue, _, spiRx, regRds) = flip execState s $ do
  memBranch <- uses memir $ \case
    Bne{} -> True
    _ -> False
  (exBranch, stall) <- execute ack regRds spiRx
  fetch stall exBranch memBranch $ decode pcValue

fetch ::Bool -> Maybe (PC 10) -> Bool -> Instr 3 10 -> State (Eul 3 10) ()
fetch stall exBranch memBranch pcValue = unless stall $ do
  pc %= updatePC exBranch pcValue
  exir .= bool pcValue Nop (isJust exBranch || memBranch)
  where
    updatePC (Just b) _ = const b
    updatePC _ Get{} = id
    updatePC _ Put{} = id
    updatePC _ _ = (+1)

execute
   :: (KnownNat n, KnownNat m)
   => Bool
   -> (Reg, Reg, Reg)
   -> Maybe Reg
   -> State (Eul n m) (Maybe (PC m), Bool)
execute ack (r1, r2, r3) spiRx = do
  instr <- use exir
  regWrite .= case instr of
    Add{} -> r1 + r2
    Sub{} -> r1 - r2
    Mul{} -> r1 * r2
    ImmH _ i -> i ++# 0
    ImmL _ i -> 0 ++# i
    Mov{} -> r1
    Put _ -> fromMaybe 0 spiRx
    Store{} -> r1
    _ -> 0
  memir .= instr
  return $ case instr of
    Bne{} | r1 /= r2 -> (Just $ unpack $ resize r3, False)
    Get _ | not ack -> (Nothing, True)
    Put _ | isNothing spiRx -> (Nothing, True)
    _  -> (Nothing, False)

regBank
  :: HiddenClockReset dom gated sync
  => (KnownNat n, 1 <= n)
  => Signal dom (Addr n)
  -> Signal dom (Addr n)
  -> Signal dom (Addr n)
  -> Signal dom (Maybe (Addr n, Reg))
  -> Signal dom (Reg, Reg, Reg)
regBank regAddr1 regAddr2 regAddr3 regWrM = bundle (regRd1, regRd2, regRd3)
  where
    regRd1 = regFile regAddr1 regWrM
    regRd2 = regFile regAddr2 regWrM
    regRd3 = regFile regAddr3 regWrM

regFile
  :: HiddenClockReset dom gated sync
  => KnownNat n
  => Signal dom (Addr n)
  -> Signal dom (Maybe (Addr n, Reg))
  -> Signal dom Reg
regFile rd wrM = mux writeThrough writeValue rdValue
  where
    rdValue = asyncRamPow2 rd wrM
    writeThrough = (Just <$> rd) .==. (fmap fst <$> wrM)
    writeValue = maybe 0 snd <$> wrM


{- OPCODE :: BitVector 4
Add   => 0
Sub   => 1
Mul   => 2
ImmH  => 3
ImmL  => 4
Bne   => 5
Mov   => 6
Get   => 7
Put   => 8
Load  => 9
Store => 10
Nop   => 11-15
-}

--   31    27   24   21   18   15               0
-- 0b[****][***][***][***][***][****************]
--  opcode|adr1|adr2|adr3|unusd|immediate OR mem[9:0]

decode :: Reg -> Instr 3 10
decode bs = case slice d31 d28 bs of
  0  -> Add   addr1 addr2 addr3
  1  -> Sub   addr1 addr2 addr3
  2  -> Mul   addr1 addr2 addr3
  3  -> ImmH  addr1 imm
  4  -> ImmL  addr1 imm
  5  -> Bne   addr1 addr2 addr3
  6  -> Mov   addr1 addr2
  7  -> Get   addr1
  8  -> Put   addr1
  9  -> Load  addr1 mem
  10 -> Store addr1 mem
  _ -> Nop
  where
    addr1 = unpack $ slice d27 d25 bs
    addr2 = unpack $ slice d24 d22 bs
    addr3 = unpack $ slice d21 d19 bs
    imm = slice d15 d0 bs
    mem = unpack $ slice d9 d0 bs

encode :: Instr 3 10 -> Reg
encode = \case
  Add   addr1 addr2 addr3 -> 0b0000 ++# pack addr1 ++# pack addr2 ++# pack addr3 ++# (0 :: BitVector 19)
  Sub   addr1 addr2 addr3 -> 0b0001 ++# pack addr1 ++# pack addr2 ++# pack addr3 ++# (0 :: BitVector 19)
  Mul   addr1 addr2 addr3 -> 0b0010 ++# pack addr1 ++# pack addr2 ++# pack addr3 ++# (0 :: BitVector 19)
  ImmH  addr1 imm         -> 0b0011 ++# pack addr1 ++# (0 :: BitVector 9) ++# imm
  ImmL  addr1 imm         -> 0b0100 ++# pack addr1 ++# (0 :: BitVector 9) ++# imm
  Bne   addr1 addr2 addr3 -> 0b0101 ++# pack addr1 ++# pack addr2 ++# pack addr3 ++# (0 :: BitVector 19)
  Mov   addr1 addr2       -> 0b0110 ++# pack addr1 ++# pack addr2 ++# (0 :: BitVector 22)
  Get   addr1             -> 0b0111 ++# pack addr1 ++# (0 :: BitVector 25)
  Put   addr1             -> 0b1000 ++# pack addr1 ++# (0 :: BitVector 25)
  Load  addr1 imm         -> 0b1001 ++# pack addr1 ++# (0 :: BitVector 15) ++# pack imm
  Store addr1 imm         -> 0b1010 ++# pack addr1 ++# (0 :: BitVector 15) ++# pack imm
  Nop                     -> 0b1111 ++# (0 :: BitVector 28)

prog :: Vec 4 (Instr 3 10)
prog =  ImmL 0 5
     :> ImmL 1 7
     :> Add  0 1 2
     :> Get  2
     :> Nil

putTest :: Vec 4 (Instr 3 10)
putTest =  ImmL 0 5
        :> Put 1
        :> Add 0 1 2
        :> Get 2
        :> Nil

fib :: Vec 13 (Instr 3 10)
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

ramTest :: Vec 12 (Instr 3 10)
ramTest =  ImmL 0 5
        :> Store 0 200
        :> ImmL 0 4
        :> Store 0 201
        :> Load 0 200
        :> Load 1 201
        :> Add 0 1 2
        :> Get 2
        :> Nil ++ jmpBegin

ramReadNew :: Vec 10 (Instr 3 10)
ramReadNew =  ImmL 0 5
           :> ImmL 1 3
           :> Add 0 1 2
           :> Store 2 200
           :> Load 0 200
           :> Get 0
           :> Nil ++ jmpBegin

jmpBegin :: Vec 4 (Instr 3 10)
jmpBegin =  ImmL 0 0
         :> ImmL 1 1
         :> ImmL 2 0
         :> Bne 0 1 2 -- Loop Back to Beginning
         :> Nil
