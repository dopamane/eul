{-# LANGUAGE LambdaCase #-}
module Eul where

import Clash.Prelude

import Control.Lens ( makeLenses, to, use, (^.), (.=), (%=) )
import Control.Monad
import Control.Monad.State

import qualified Data.List as L

type DomSpi = 'Dom "Spi" 166667 -- freqCalc 6e6  (6mhz)

data Op a
  = Add a a
  | Sub a a

data Spi r t
  = Spi { _rx  :: Vec r Bit
        , _tx  :: Vec t Bit
        , _ss' :: Bool
        }
makeLenses ''Spi

data Stage = Read | Write | Wait

data Eul r t
  = Eul { _stage :: Stage
        , _spi   :: Spi r t
        }
makeLenses ''Eul

{-# ANN topEntity
  (Synthesize
    { t_name = "EUL"
    , t_inputs = [ PortName "SCK"
                 , PortName "rst"
                 , PortName "SS"
                 , PortName "MOSI"
                 ]
    , t_output = PortName "MISO"
    })#-}

topEntity
  :: Clock DomSpi 'Source -- sck
  -> Reset DomSpi 'Synchronous
  -> Signal DomSpi Bool   -- ss
  -> Signal DomSpi Bit    -- mosi
  -> Signal DomSpi Bit    -- miso
topEntity sck rst = withClockReset sck rst eul
  --where
    --rst = unsafeToSyncReset $ pure False
{-# NOINLINE topEntity #-}

eul
  :: HiddenClockReset dom gated sync
  => Signal dom Bool
  -> Signal dom Bit
  -> Signal dom Bit
eul ss mosi = eulO <$> eulReg
  where
    eulReg = register initial $ eulT <$> eulReg <*> ss <*> mosi
    initial = Eul Read (Spi (replicate d24 0) (replicate d16 0) True)

eulO :: Eul r (t+1) -> Bit
eulO s = s^.spi.tx.to head

eulT :: Eul 24 16 -> Bool -> Bit -> Eul 24 16
eulT s ss mosi = flip execState s $ do
  use stage >>= \case
    Read  -> spiRead ss mosi
    Write -> spiWrite ss
    Wait  -> put $ Eul Read (Spi (repeat 0) (repeat 0) True)
  spi.ss' .= ss

spiRead :: Bool -> Bit -> State (Eul 24 16) ()
spiRead ss mosi = do
  s <- use spi
  if s^.ss'.to not && ss then do
    stage .= Write
    spi.tx .= calc (s^.rx.to v2bv)
  else unless ss $ spi.rx %= (<<+ mosi)

spiWrite :: (KnownNat r, KnownNat t) => Bool -> State (Eul r t) ()
spiWrite ss = do
  s <- use spi
  if s^.ss'.to not && ss then
    stage .= Wait
  else unless ss $ spi.tx %= flip rotateLeftS d1

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

process :: Maybe (Unsigned 8) -> Vec 16 Bit
process = bv2v . maybe 0 ((1 ++#) . pack)

calc :: BitVector 24 -> Vec 16 Bit
calc = process . alu

eulTest :: [Bit]
eulTest = sampleN 43 $ eul ss mosi
  where
    ss = fromList $ [True] L.++ L.replicate 24 False L.++ [True, True] L.++ L.replicate 16 False
    mosi = fromList $ [0, 0,0,0,0,0,0,0,1, 0,0,0,0,0,0,1,1, 0,0,0,0,0,0,0,1] L.++ L.replicate 18 0
