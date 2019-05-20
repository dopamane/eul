module Eul where

import Clash.Prelude

type DomSpi = 'Dom "Spi" 166667 -- freqCalc 6e6  (6mhz)

{-# ANN topEntity
  (Synthesize
    { t_name = "EUL"
    , t_inputs = [ PortName "SCK"
                 , PortName "SS"
                 , PortName "MOSI"
                 ]
    , t_output = PortName "MISO"
    })#-}

topEntity
  :: Clock DomSpi 'Source -- sck
  -> Signal DomSpi Bool   -- ss
  -> Signal DomSpi Bit    -- mosi
  -> Signal DomSpi Bit    -- miso
topEntity sck = withClockReset sck rst eul
  where
    rst = unsafeToSyncReset $ pure False
{-# NOINLINE topEntity #-}

eul
  :: HiddenClockReset dom gated sync
  => Signal dom Bool
  -> Signal dom Bit
  -> Signal dom Bit
eul = curry (mooreB eulT eulO initial)
  where
    initial = undefined

eulO = undefined

eulT = undefined
