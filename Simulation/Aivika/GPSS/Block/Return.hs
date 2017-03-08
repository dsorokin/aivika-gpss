
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Return
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block RETURN.
--
module Simulation.Aivika.GPSS.Block.Return
       (returnBlock) where

import Simulation.Aivika
import Simulation.Aivika.GPSS.Transact
import Simulation.Aivika.GPSS.Block
import Simulation.Aivika.GPSS.Facility

-- | This is an analog of the GPSS construct
--
-- @RETURN A@
--
-- Note that unlike GPSS the return of Facility has immediate effect, which means
-- that another transact may capture the Facility before the current transact
-- finishes leaving the block.
returnBlock :: Facility a
               -- ^ the facility
               -> Block (Transact a) (Transact a)
returnBlock r =
  Block { blockProcess = \a -> returnFacility r a >> return a }
