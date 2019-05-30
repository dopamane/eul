{-# LANGUAGE LambdaCase #-}
module Eul where

import Clash.Prelude

import Rstn ( rstn )
import Spi  ( spiWorker )

--import qualified Data.List as L

data Op a
  = Add a a
  | Sub a a

data Stage = Read | Write

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
    txLd = mealyB eulT initial (ack, rx)
    initial = (Read, 0 :: BitVector 16)

eulT
  :: (Stage, BitVector 16)
  -> (Bool, Maybe (BitVector 24))
  -> ((Stage, BitVector 16), Maybe (BitVector 16))
eulT (Read, _) (_, Nothing) = ((Read, 0), Nothing)
eulT (Read, _) (_, Just r) = ((Write, calc r), Nothing)
eulT (Write, w) (False, _) = ((Write, w), Just w)
eulT (Write, w) (True, _)  = ((Read, 0), Just w)

decode :: BitVector 24 -> Maybe (Op (Unsigned 8))
decode bs = case slice d23 d16 bs of
  0 -> Just $ Add operand1 operand2
  1 -> Just $ Sub operand1 operand2
  _ -> Nothing
  where
    operand1 = unpack $ slice d15 d8 bs
    operand2 = unpack $ slice d7  d0 bs

execute :: Num a => Op a -> a
execute = \case
  Add a b -> a + b
  Sub a b -> a - b

alu :: BitVector 24 -> Maybe (Unsigned 8)
alu = fmap execute . decode

process :: Maybe (Unsigned 8) -> BitVector 16
process = maybe 0 ((1 ++#) . pack)

calc :: BitVector 24 -> BitVector 16
calc = process . alu

{-
eulTest :: [Bit]
eulTest = sampleN 43 $ eul ss mosi
  where
    ss = fromList $ [True] L.++ L.replicate 24 False L.++ [True, True] L.++ L.replicate 16 False
    mosi = fromList $ [0, 0,0,0,0,0,0,0,1, 0,0,0,0,0,0,1,1, 0,0,0,0,0,0,0,1] L.++ L.replicate 18 0
-}
