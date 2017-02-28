
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Release
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block RELEASE.
--
module Simulation.Aivika.Trans.GPSS.Block.Release
       (releaseBlock) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Transact
import Simulation.Aivika.Trans.GPSS.Block
import Simulation.Aivika.Trans.GPSS.Facility

-- | This is the GPSS construct
--
-- @RELEASE A@
releaseBlock :: MonadDES m
                => Facility m a
                -- ^ the facility
                -> Block m (Transact m a) (Transact m a)
releaseBlock r =
  Block { blockProcess = \a -> releaseFacility r a >> return a }
