module Rstn (rstn) where

import GHC.Stack                    ( HasCallStack )
import Clash.Prelude
import Clash.Annotations.Primitive

{-# ANN module (Primitive Verilog "RSTN.json") #-}

{-# NOINLINE rstn #-}
rstn
  :: HasCallStack
  => Clock dom gated
  -> Signal dom Bit
rstn _ = pure 0
