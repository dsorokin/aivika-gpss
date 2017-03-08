
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Release
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block RELEASE.
--
module Simulation.Aivika.GPSS.Block.Release
       (releaseBlock) where

import Simulation.Aivika
import Simulation.Aivika.GPSS.Transact
import Simulation.Aivika.GPSS.Block
import Simulation.Aivika.GPSS.Facility

-- | This is an analog of the GPSS construct
--
-- @RELEASE A@
--
-- Note that unlike GPSS the releasing of Facility has immediate effect, which means
-- that another transact may capture the Facility before the current transact
-- finishes leaving the block.
releaseBlock :: Facility a
                -- ^ the facility
                -> Block (Transact a) (Transact a)
releaseBlock r =
  Block { blockProcess = \a -> releaseFacility r a >> return a }
