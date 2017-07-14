
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Priority
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block PRIORITY.
--
module Simulation.Aivika.Trans.GPSS.Block.Priority
       (priorityBlock) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Block
import Simulation.Aivika.Trans.GPSS.Transact

-- | This is the GPSS construct
--
-- @PRIORITY A@
priorityBlock :: MonadDES m 
                 => Int
                 -- ^ the priority
                 -> Block m (Transact m a) (Transact m a)
{-# INLINABLE priorityBlock #-}
priorityBlock priority =
  Block { blockProcess = \a -> return (assignTransactPriority a priority) }
