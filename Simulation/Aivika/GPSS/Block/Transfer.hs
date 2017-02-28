
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Transfer
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block TRANSFER.
--
module Simulation.Aivika.GPSS.Block.Transfer
       (transferBlock) where

import Simulation.Aivika
import Simulation.Aivika.GPSS.Block

-- | This is the GPSS construct
--
-- @TRANSFER ,New_Place@
transferBlock :: Block a ()
                 -- ^ a new place
                 -> Block a b
transferBlock x =
  Block { blockProcess = \a -> transferProcess (blockProcess x a) }
