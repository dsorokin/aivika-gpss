
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Transfer
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block TRANSFER.
--
module Simulation.Aivika.Trans.GPSS.Block.Transfer
       (transferBlock) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Block

-- | This is the GPSS construct
--
-- @TRANSFER ,New_Place@
transferBlock :: MonadDES m
                 => Block m a ()
                 -- ^ a new place
                 -> Block m a b
{-# INLINABLE transferBlock #-}
transferBlock x =
  Block { blockProcess = \a -> transferProcess (blockProcess x a) }
