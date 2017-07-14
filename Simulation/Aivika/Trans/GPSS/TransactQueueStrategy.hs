
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.GPSS.TransactQueueStrategy
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines a GPSS transact queue strategy.
--
module Simulation.Aivika.Trans.GPSS.TransactQueueStrategy
       (TransactQueueStrategy(..),
        transactStrategyQueueDeleteBy,
        transactStrategyQueueContainsBy) where

import Control.Monad
import Control.Monad.Trans

import Data.IORef
import qualified Data.IntMap as M

import Simulation.Aivika.Trans
import qualified Simulation.Aivika.Trans.DoubleLinkedList as DLL

-- | The transact queue strategy.
data TransactQueueStrategy s = TransactQueueStrategy s

-- | An implementation of the 'QueueStrategy' class.
instance MonadDES m => QueueStrategy m (TransactQueueStrategy s) where

  -- | A queue used by the 'TransactQueueStrategy' strategy.
  data StrategyQueue m (TransactQueueStrategy s) a =
    TransactStrategyQueue { transactStrategy :: TransactQueueStrategy s,
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

instance MonadDES m => DequeueStrategy m (TransactQueueStrategy FCFS) where

  {-# INLINABLE strategyDequeue #-}
  strategyDequeue q =
    do m <- readRef (transactStrategyQueue q)
       let (k, xs) = M.findMin m
       i <- DLL.listFirst xs
       DLL.listRemoveFirst xs
       empty <- DLL.listNull xs
       when empty $
         modifyRef (transactStrategyQueue q) $
         M.delete k
       return i

instance MonadDES m => DequeueStrategy m (TransactQueueStrategy LCFS) where

  {-# INLINABLE strategyDequeue #-}
  strategyDequeue q =
    do m <- readRef (transactStrategyQueue q)
       let (k, xs) = M.findMin m
       i <- DLL.listLast xs
       DLL.listRemoveLast xs
       empty <- DLL.listNull xs
       when empty $
         modifyRef (transactStrategyQueue q) $
         M.delete k
       return i

instance (MonadDES m, DequeueStrategy m (TransactQueueStrategy s)) => PriorityQueueStrategy m (TransactQueueStrategy s) Int where

  {-# INLINABLE strategyEnqueueWithPriority #-}
  strategyEnqueueWithPriority q priority i =
    do m <- readRef (transactStrategyQueue q)
       let k  = - priority
           xs = M.lookup k m
       case xs of
         Nothing ->
           do xs <- liftSimulation DLL.newList
              DLL.listAddLast xs i
              modifyRef (transactStrategyQueue q) $
                M.insert k xs
         Just xs ->
           DLL.listAddLast xs i

instance MonadDES m => DeletingQueueStrategy m (TransactQueueStrategy FCFS) where

  {-# INLINABLE strategyQueueDeleteBy #-}
  strategyQueueDeleteBy q pred =
    do m <- readRef (transactStrategyQueue q)
       let loop [] = return Nothing
           loop ((k, xs): tail) =
             do a <- DLL.listRemoveBy xs pred
                case a of
                  Nothing -> loop tail
                  Just _  ->
                    do empty <- DLL.listNull xs
                       when empty $
                         modifyRef (transactStrategyQueue q) $
                         M.delete k
                       return a
       loop (M.assocs m)

  {-# INLINABLE strategyQueueContainsBy #-}
  strategyQueueContainsBy q pred =
    do m <- readRef (transactStrategyQueue q)
       let loop [] = return Nothing
           loop ((k, xs): tail) =
             do a <- DLL.listContainsBy xs pred
                case a of
                  Nothing -> loop tail
                  Just _  -> return a
       loop (M.assocs m)

-- | Try to delete the transact by the specified priority and satisfying to the provided predicate.
transactStrategyQueueDeleteBy :: MonadDES m
                                 => StrategyQueue m (TransactQueueStrategy s) a
                                 -- ^ the queue
                                 -> Int
                                 -- ^ the transact priority
                                 -> (a -> Bool)
                                 -- ^ the predicate
                                 -> Event m (Maybe a)
{-# INLINABLE transactStrategyQueueDeleteBy #-}
transactStrategyQueueDeleteBy q priority pred =
  do m <- readRef (transactStrategyQueue q)
     let k  = - priority
         xs = M.lookup k m
     case xs of
       Nothing -> return Nothing
       Just xs ->
         do a <- DLL.listRemoveBy xs pred
            empty <- DLL.listNull xs
            when empty $
              modifyRef (transactStrategyQueue q) $
              M.delete k
            return a

-- | Test whether the queue contains a transact with the specified priority satisfying the provided predicate.
transactStrategyQueueContainsBy :: MonadDES m
                                   => StrategyQueue m (TransactQueueStrategy s) a
                                   -- ^ the queue
                                   -> Int
                                   -- ^ the transact priority
                                   -> (a -> Bool)
                                   -- ^ the predicate
                                   -> Event m (Maybe a)
{-# INLINABLE transactStrategyQueueContainsBy #-}
transactStrategyQueueContainsBy q priority pred =
  do m <- readRef (transactStrategyQueue q)
     let k  = - priority
         xs = M.lookup k m
     case xs of
       Nothing -> return Nothing
       Just xs -> DLL.listContainsBy xs pred
