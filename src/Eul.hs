module Eul where

import Clash.Prelude

import Control.Lens ( makeLenses, use, (^.), (.=), (+=), (%=) )
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
  | Bne Addr Addr Addr
  | Mov Addr Addr
  | Get Addr
  | Nop

data Stage = Fetch | Execute | Write

data Eul n = Eul
  { _instr :: Instr
  , _stage :: Stage
  , _regs  :: RegBank
  , _pc    :: Unsigned n
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
topEntity clk = withClockReset clk rst (eul fib)
  where
    rst = rstn d16 clk
{-# NOINLINE topEntity #-}

eul
  :: HiddenClockReset dom gated sync
  => KnownNat n
  => Vec (2^n) Instr
  -> Signal dom Bit
  -> Signal dom Bool
  -> Signal dom Bit
eul romContent sck ss = miso
  where
    (miso, ack) = spiWorkerTx txLd sck ss
    (txLd, romAddr) = mooreB eulT eulO initial (ack, romValue)
    initial = Eul Nop Fetch (repeat 0) 0
    romValue = romPow2 romContent romAddr

eulO :: Eul n -> (Maybe (BitVector 32), Unsigned n)
eulO s@Eul{_instr=(Get i), _stage=Write, _regs=r} = (Just $ r !! i, _pc s)
eulO s = (Nothing, _pc s)

eulT :: KnownNat n => Eul n -> (Bool, Instr) -> Eul n
eulT s (ack, romValue) = flip execState s $ case s^.stage of
  Fetch   -> stage .= Execute
  Execute -> execute romValue
  Write   -> when ack $ stage .= Fetch

execute :: KnownNat n => Instr -> State (Eul n) ()
execute ins = do
  r     <- use regs
  curPC <- use pc
  instr .= ins
  regs %= case ins of
    Add  a b c -> replace c $ (r !! a) + (r !! b)
    Sub  a b c -> replace c $ (r !! a) - (r !! b)
    Mul  a b c -> replace c $ (r !! a) * (r !! b)
    PutH a i   -> replace a $ i ++# getLower (r !! a)
    PutL a i   -> replace a $ getHigher (r !! a) ++# i
    Mov a b    -> replace b $ r !! a
    _          -> id
  stage .= case ins of
    Get _ -> Write
    _     -> Fetch
  case ins of
    Bne a b pcRegAddr | (r !! a) /= (r !! b) -> pc .= unpack (resize $ r !! pcRegAddr)
    _  -> unless (curPC == maxBound) $ pc += 1
  where
    getHigher = slice d31 d16
    getLower  = slice d15 d0

prog :: Vec 8 Instr
prog =  PutL 0 5
     :> PutL 1 7
     :> Add  0 1 2
     :> Get  2
     :> Nop
     :> Nil ++ repeat Nop

fib :: Vec 16 Instr
fib =  PutL 0 29 -- nth  fibonacci number 10 -> r0
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
    :> Bne 3 0 5 -- goto LOOP BEGIN if i /= n
    :> Get 2     -- spi write
    :> Nop       -- END
    :> Nil ++ repeat Nop
