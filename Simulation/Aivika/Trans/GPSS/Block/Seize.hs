
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Seize
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block SEIZE.
--
module Simulation.Aivika.Trans.GPSS.Block.Seize
       (seizeBlock) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Transact
import Simulation.Aivika.Trans.GPSS.Block
import Simulation.Aivika.Trans.GPSS.Facility

-- | This is the GPSS construct
--
-- @SEIZE A@
seizeBlock :: MonadDES m
              => Facility m a
              -- ^ the facility
              -> Block m (Transact m a) (Transact m a)
{-# INLINABLE seizeBlock #-}
seizeBlock r =
  Block { blockProcess = \a -> seizeFacility r a >> return a }
