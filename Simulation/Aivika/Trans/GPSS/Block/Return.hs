
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Return
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block RETURN.
--
module Simulation.Aivika.Trans.GPSS.Block.Return
       (returnBlock) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Transact
import Simulation.Aivika.Trans.GPSS.Block
import Simulation.Aivika.Trans.GPSS.Facility

-- | This is an analog of the GPSS construct
--
-- @RETURN A@
--
-- Note that unlike GPSS the return of Facility has immediate effect, which means
-- that another transact may capture the Facility before the current transact
-- finishes leaving the block.
returnBlock :: MonadDES m
               => Facility m a
               -- ^ the facility
               -> Block m (Transact m a) (Transact m a)
{-# INLINABLE returnBlock #-}
returnBlock r =
  Block { blockProcess = \a -> returnFacility r a >> return a }
