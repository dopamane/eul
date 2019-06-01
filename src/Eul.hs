module Eul where

import Clash.Prelude

import Control.Lens ( makeLenses, use, (.=), (%=) )
import Control.Monad.State
import Data.Maybe ( isJust )
import Data.Tuple ( swap )
import Data.Bool ( bool )

import Rstn ( rstn )
import Spi  ( spiWorkerTx )

type Addr n    = Index n
type Reg       = BitVector 32
type RegBank n = Vec n Reg
type Imm       = BitVector 16
type Ram n     = Unsigned n
type Rom n     = Unsigned n

data Instr n m
  = Add   (Addr n) (Addr n) (Addr n)
  | Sub   (Addr n) (Addr n) (Addr n)
  | Mul   (Addr n) (Addr n) (Addr n)
  | PutH  (Addr n) Imm
  | PutL  (Addr n) Imm
  | Bne   (Addr n) (Addr n) (Addr n)
  | Mov   (Addr n) (Addr n)
  | Load  (Addr n) (Ram m)
  | Store (Addr n) (Ram m)
  | Get   (Addr n)
  | Nop

data Eul n m p = Eul
  { _exir  :: Instr n m
  , _memir :: Instr n m
  , _regs  :: RegBank n
  , _pc    :: Unsigned p
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
  => KnownNat p
  => Vec (2^p) (Instr 8 8)
  -> Signal dom Bit
  -> Signal dom Bool
  -> Signal dom Bit
eul romContent sck ss = miso
  where
    (miso, ack) = spiWorkerTx txLd sck ss
    (txLd, romAddr, rdAddr, wrM) = mealyB eulT initial (ack, romValue, memValue)
    initial = Eul Nop Nop (replicate d8 0) 0
    romValue = romPow2 romContent romAddr
    memValue = readNew (blockRamPow2 (replicate d256 0)) rdAddr wrM

eulT
  :: (KnownNat n, KnownNat m, KnownNat p)
  => Eul n m p
  -> (Bool, Instr n m, Reg)
  -> (Eul n m p, (Maybe Reg, Rom p, Ram m, Maybe (Ram m, Reg)))
eulT s (ack, romValue, ramValue) = swap $ flip runState s $ do
  (wrM, ldReg) <- memory ramValue
  (stall, spiTx, branch, rdAddr) <- execute ack ldReg
  nextPC <- fetch stall branch romValue
  return (spiTx, nextPC, rdAddr, wrM)

fetch
  :: (KnownNat m, KnownNat p)
  => Bool
  -> Maybe (Rom p)
  -> Instr n m
  -> State (Eul n m p) (Rom p)
fetch stall branch romValue = do
  curPC <- use pc
  unless stall $ do
    pc %= updatePC branch
    exir .= bool romValue Nop (isJust branch)
  return curPC
  where
    updatePC Nothing curPC | curPC == maxBound = curPC
                           | otherwise = curPC + 1
    updatePC (Just b) _  = b

execute
  :: (KnownNat n, KnownNat m, KnownNat p)
  => Bool
  -> Maybe (Addr n, Reg)
  -> State (Eul n m p) (Bool, Maybe Reg, Maybe (Rom p), Ram m)
execute ack ldReg = do
  r <- use regs
  instr <- use exir
  regs %= case ldReg of
    Just (a, i) -> replace a i
    Nothing     -> id
  regs %= case instr of
    Add  a b c -> replace c $ (r !! a) + (r !! b)
    Sub  a b c -> replace c $ (r !! a) - (r !! b)
    Mul  a b c -> replace c $ (r !! a) * (r !! b)
    PutH a i   -> replace a $ i ++# getLower (r !! a)
    PutL a i   -> replace a $ getHigher (r !! a) ++# i
    Mov  a b   -> replace b $ r !! a
    _          -> id
  memir .= instr
  return $ case instr of
    Bne a b pc' | (r !! a) /= (r !! b) -> (False, Nothing, Just $ unpack $ resize $ r !! pc', 0)
    Load _ m -> (False, Nothing, Nothing, m)
    Get a | not ack -> (True, Just $ r !! a, Nothing, 0)
    _ -> (False, Nothing, Nothing, 0)
  where
    getHigher = slice d31 d16
    getLower  = slice d15 d0

memory
  :: KnownNat n
  => Reg
  -> State (Eul n m p) (Maybe (Ram m, Reg), Maybe (Addr n, Reg))
memory ramValue = do
  ir <- use memir
  r <- use regs
  return $ case ir of
    Store a m -> (Just (m, r !! a), Nothing)
    Load a _ -> (Nothing, Just (a, ramValue))
    _ -> (Nothing, Nothing)

prog :: Vec 8 (Instr 8 8)
prog =  Nop
     :> PutL 0 5
     :> PutL 1 7
     :> Add  0 1 2
     :> Get  2
     :> Nop
     :> Nop
     :> Nop
     :> Nil

fib :: Vec 16 (Instr 8 8)
fib =  Nop
    :> PutL 0 29 -- nth  fibonacci number 10 -> r0
    :> PutL 1 0  -- prev prev              0 -> r1
    :> PutL 2 1  -- prev                   1 -> r2
    :> PutL 3 2  -- i                      2 -> r3
    :> PutL 4 1  -- increment              1 -> r4
    :> PutL 5 7  -- loop begin addr        7 -> r5
    :> Add 1 2 6 -- prev prev + prev r1 + r2 -> r6 LOOP BEGIN
    :> Mov 2 1   -- prev -> prev prev     r2 -> r1
    :> Mov 6 2   -- fib -> prev           r6 -> r2
    :> Add 3 4 7 -- i + 1            r3 + r4 -> r7
    :> Mov 7 3   --                       r7 -> r3
    :> Bne 3 0 5 -- goto LOOP BEGIN if i /= n
    :> Get 2     -- spi write
    :> Nop       -- END
    :> Nop
    :> Nil

ramTest :: Vec 16 (Instr 8 8)
ramTest =  Nop
        :> PutL 0 5
        :> Store 0 0
        :> PutL 0 4
        :> Store 0 1
        :> Load 0 0
        :> Load 1 1
        :> Add 0 1 2
        :> Get 2
        :> Nil ++ repeat Nop
