
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Priority
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block PRIORITY.
--
module Simulation.Aivika.GPSS.Block.Priority
       (priorityBlock) where

import Simulation.Aivika
import Simulation.Aivika.GPSS.Block
import Simulation.Aivika.GPSS.Transact

-- | This is the GPSS construct
--
-- @PRIORITY A@
priorityBlock :: Int
                 -- ^ the priority
                 -> Block (Transact a) (Transact a)
priorityBlock priority =
  Block { blockProcess = \a -> return (assignTransactPriority a priority) }
