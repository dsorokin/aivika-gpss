
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Return
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
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

-- | This is the GPSS construct
--
-- @RETURN A@
returnBlock :: Facility a
               -- ^ the facility
               -> Block (Transact a) (Transact a)
returnBlock r =
  Block { blockProcess = \a -> returnFacility r a >> return a }
