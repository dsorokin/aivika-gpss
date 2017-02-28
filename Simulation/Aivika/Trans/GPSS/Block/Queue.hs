
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Queue
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block QUEUE.
--
module Simulation.Aivika.Trans.GPSS.Block.Queue
       (queueBlock) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Transact
import Simulation.Aivika.Trans.GPSS.Block
import qualified Simulation.Aivika.Trans.GPSS.Queue as Q

-- | This is the GPSS construct
--
-- @QUEUE WaitingLine@
queueBlock :: MonadDES m
              => Q.Queue m
              -- ^ the queue
              -> Int
              -- ^ the content increment
              -> Block m (Transact m a) (Transact m a)
{-# INLINABLE queueBlock #-}
queueBlock q increment =
  Block { blockProcess = \a -> (liftEvent $ Q.enqueue q a increment) >> return a }
