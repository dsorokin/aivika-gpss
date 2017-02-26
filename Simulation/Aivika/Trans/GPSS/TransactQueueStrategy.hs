
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Trans.GPSS.TransactQueueStrategy
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines a GPSS transact queue strategy.
--
module Simulation.Aivika.Trans.GPSS.TransactQueueStrategy
       (TransactQueueStrategy(..)) where

import Control.Monad
import Control.Monad.Trans

import qualified Data.IntMap as M

import Simulation.Aivika.Trans
import qualified Simulation.Aivika.Trans.DoubleLinkedList as DLL

-- | The transact queue strategy.
data TransactQueueStrategy = InnerFCFSTransactQueueStrategy
                             -- ^ sorted by priority but then by FCSF (FIFO)
                             -- within transacts of the same priority
                           | InnerLCFSTransactQueueStrategy
                             -- ^ sorted by priority but then by LCFS (LIFO)
                             -- within transacts of the same priority

-- | An implementation of the 'QueueStrategy' class.
instance MonadDES m => QueueStrategy m TransactQueueStrategy where

  -- | A queue used by the 'TransactQueueStrategy' strategy.
  data StrategyQueue m TransactQueueStrategy a =
    TransactStrategyQueue { transactStrategy :: TransactQueueStrategy,
                            -- ^ the strategy itself
                            transactStrategyQueue :: Ref m (M.IntMap (DLL.DoubleLinkedList m a))
                            -- ^ the transact queue
                          }

  {-# INLINABLE newStrategyQueue #-}
  newStrategyQueue s =
    do r <- newRef M.empty
       return $ TransactStrategyQueue s r

  {-# INLINABLE strategyQueueNull #-}
  strategyQueueNull q =
    do m <- readRef (transactStrategyQueue q)
       return $ M.null m

instance MonadDES m => DequeueStrategy m TransactQueueStrategy where

  {-# INLINABLE strategyDequeue #-}
  strategyDequeue q =
    do m <- readRef (transactStrategyQueue q)
       let (p, xs) = M.findMax m
           extract =
             case transactStrategy q of
               InnerFCFSTransactQueueStrategy ->
                 do i <- DLL.listFirst xs
                    DLL.listRemoveFirst xs
                    return i
               InnerLCFSTransactQueueStrategy ->
                 do i <- DLL.listLast xs
                    DLL.listRemoveLast xs
                    return i
       i <- extract
       empty <- DLL.listNull xs
       when empty $
         modifyRef (transactStrategyQueue q) $
         M.delete p
       return i

instance MonadDES m => PriorityQueueStrategy m TransactQueueStrategy Int where

  {-# INLINABLE strategyEnqueueWithPriority #-}
  strategyEnqueueWithPriority q p i =
    do m <- readRef (transactStrategyQueue q)
       let xs = M.lookup p m
       case xs of
         Nothing ->
           do xs <- liftSimulation DLL.newList
              DLL.listAddLast xs i
         Just xs ->
           DLL.listAddLast xs i
