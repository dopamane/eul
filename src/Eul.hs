{-# LANGUAGE LambdaCase #-}
module Eul where

import Clash.Prelude

import Control.Lens ( makeLenses, use, (^.), (.=) )
import Control.Monad.State 
import Data.Maybe ( isJust, fromMaybe )

import Rstn ( rstn )
import Spi  ( spiWorker )

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
  | Get Addr

-- Add  => 0b000 => 0
-- Sub  => 0b001 => 1
-- Mul  => 0b010 => 2
-- putH => 0b011 => 3 
-- putL => 0b100 => 4
-- Get  => 0b101 => 5

data Stage = Fetch | Execute | Write

data Eul = Eul
  { _instr :: Instr
  , _stage :: Stage
  , _regs  :: RegBank
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
topEntity clk = withClockReset clk rst eul
  where
    rst = rstn d16 clk
{-# NOINLINE topEntity #-}

eul
  :: HiddenClockReset dom gated sync
  => Signal dom Bit
  -> Signal dom Bool
  -> Signal dom Bit
  -> Signal dom Bit
eul sck ss mosi = miso
  where
    (miso, ack, rx) = spiWorker txLd sck ss mosi
    txLd = mooreB eulT writeBack initial (ack, rx)
    initial = Eul (Get 0) Fetch $ repeat 0

writeBack :: Eul -> Maybe (BitVector 32)
writeBack Eul{_instr=(Get i), _stage=Write, _regs=r} = Just $ r !! i
writeBack _ = Nothing

eulT
  :: Eul
  -> (Bool, Maybe (BitVector 32))
  -> Eul
eulT s (ack, rx) = flip execState s $ case s^.stage of
  Fetch   -> fetch rx
  Execute -> execute
  Write   -> when ack $ stage .= Fetch 
    
fetch :: Maybe (BitVector 32) -> State Eul ()
fetch Nothing = return ()
fetch (Just rx) = do
  let decoded = decode rx
  instr .= fromMaybe (Get 0) decoded
  when (isJust decoded) $ stage .= Execute  

execute :: State Eul ()
execute = do
  r <- use regs
  ins <- use instr
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
  5 -> Just $ Get  addr1 
  _ -> Nothing
  where
    addr1 = unpack $ slice d28 d26 bs
    addr2 = unpack $ slice d25 d23 bs
    addr3 = unpack $ slice d22 d20 bs
    imm   = slice d15 d0 bs



