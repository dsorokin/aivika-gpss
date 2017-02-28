
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

-- | This is the GPSS construct
--
-- @RETURN A@
returnBlock :: MonadDES m
               => Facility m a
               -- ^ the facility
               -> Block m (Transact m a) (Transact m a)
{-# INLINABLE returnBlock #-}
returnBlock r =
  Block { blockProcess = \a -> returnFacility r a >> return a }
