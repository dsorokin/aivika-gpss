
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Seize
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block SEIZE.
--
module Simulation.Aivika.GPSS.Block.Seize
       (seizeBlock) where

import Simulation.Aivika
import Simulation.Aivika.GPSS.Transact
import Simulation.Aivika.GPSS.Block
import Simulation.Aivika.GPSS.Facility

-- | This is the GPSS construct
--
-- @SEIZE A@
seizeBlock :: Facility a
              -- ^ the facility
              -> Block (Transact a) (Transact a)
seizeBlock r =
  Block { blockProcess = \a -> seizeFacility r a >> return a }
