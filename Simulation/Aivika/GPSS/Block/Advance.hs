
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Advance
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block ADVANCE.
--
module Simulation.Aivika.GPSS.Block.Advance
       (advanceBlock) where

import Simulation.Aivika
import Simulation.Aivika.GPSS.Block

-- | This is the GPSS construct
--
-- @ADVANCE A,B@
advanceBlock :: Process ()
                -- ^ the delay
                -> Block a a
advanceBlock p =
  Block { blockProcess = \a -> p >> return a }
