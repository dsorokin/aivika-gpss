
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.GPSS.TransactQueue
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines a GPSS transact queue.
--
module Simulation.Aivika.Trans.GPSS.TransactQueue
       (TransactQueue,
        TransactQueueStrategy,
        newTransactQueue) where

import Control.Monad
import Control.Monad.Trans

import qualified Data.IntMap as M

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Queue.Infinite.Base
import qualified Simulation.Aivika.Trans.DoubleLinkedList as DLL

import Simulation.Aivika.GPSS.Transact

-- | A transact queue.
type TransactQueue m a = Queue m FCFS TransactQueueStrategy (Transact a)

-- | The transact queue strategy.
data TransactQueueStrategy = TransactQueueStrategy

-- | An implementation of the 'QueueStrategy' class.
instance MonadDES m => QueueStrategy m TransactQueueStrategy where

  -- | A queue used by the 'TransactQueueStrategy' strategy.
  data StrategyQueue m TransactQueueStrategy a =
    TransactStrategyQueue (Ref m (M.IntMap (DLL.DoubleLinkedList m a)))

  {-# INLINABLE newStrategyQueue #-}
  newStrategyQueue s =
    do r <- newRef M.empty
       return $ TransactStrategyQueue r

  {-# INLINABLE strategyQueueNull #-}
  strategyQueueNull (TransactStrategyQueue r) =
    do m <- readRef r
       return $ M.null m

instance MonadDES m => DequeueStrategy m TransactQueueStrategy where

  {-# INLINABLE strategyDequeue #-}
  strategyDequeue (TransactStrategyQueue r) =
    do m <- readRef r
       let (p, xs) = M.findMax m
       i <- DLL.listFirst xs
       DLL.listRemoveFirst xs
       empty <- DLL.listNull xs
       when empty $
         modifyRef r $
         M.delete p
       return i

instance MonadDES m => PriorityQueueStrategy m TransactQueueStrategy Int where

  {-# INLINABLE strategyEnqueueWithPriority #-}
  strategyEnqueueWithPriority (TransactStrategyQueue r) p i =
    do m <- readRef r
       let xs = M.lookup p m
       case xs of
         Nothing ->
           do xs <- liftSimulation DLL.newList
              DLL.listAddLast xs i
         Just xs ->
           DLL.listAddLast xs i

-- | Create a new transact queue
newTransactQueue :: MonadDES m => Simulation m (TransactQueue m a)
{-# INLINABLE newTransactQueue #-}
newTransactQueue = newQueue FCFS TransactQueueStrategy
