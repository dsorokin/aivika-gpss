
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Assign
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block ASSIGN.
--
module Simulation.Aivika.Trans.GPSS.Block.Assign
       (assignBlock,
        assignBlockM) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Block
import Simulation.Aivika.Trans.GPSS.Transact

-- | This is the GPSS construct
--
-- @ASSIGN A,B,C@
assignBlock :: MonadDES m
               => (a -> b)
               -- ^ the transform
               -> Block m (Transact m a) (Transact m b)
{-# INLINABLE assignBlock #-}
assignBlock f =
  Block { blockProcess = \a -> return (assignTransactValue a f) }

-- | This is the GPSS construct
--
-- @ASSIGN A,B,C@
assignBlockM :: MonadDES m =>
                (a -> Process m b)
                -- ^ the transform computation
                -> Block m (Transact m a) (Transact m b)
{-# INLINABLE assignBlockM #-}
assignBlockM f =
  Block { blockProcess = \a -> assignTransactValueM a f }
