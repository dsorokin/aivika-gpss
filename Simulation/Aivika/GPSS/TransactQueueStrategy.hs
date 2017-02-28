
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.GPSS.TransactQueueStrategy
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines a GPSS transact queue strategy.
--
module Simulation.Aivika.GPSS.TransactQueueStrategy
       (TransactQueueStrategy(..),
        transactStrategyQueueDeleteBy,
        transactStrategyQueueContainsBy) where

import Control.Monad
import Control.Monad.Trans

import Data.IORef
import qualified Data.IntMap as M

import Simulation.Aivika
import qualified Simulation.Aivika.DoubleLinkedList as DLL

-- | The transact queue strategy.
data TransactQueueStrategy = InnerFCFSTransactQueueStrategy
                             -- ^ sorted by priority but then by FCSF (FIFO)
                             -- within transacts of the same priority
                           | InnerLCFSTransactQueueStrategy
                             -- ^ sorted by priority but then by LCFS (LIFO)
                             -- within transacts of the same priority

-- | An implementation of the 'QueueStrategy' class.
instance QueueStrategy TransactQueueStrategy where

  -- | A queue used by the 'TransactQueueStrategy' strategy.
  data StrategyQueue TransactQueueStrategy a =
    TransactStrategyQueue { transactStrategy :: TransactQueueStrategy,
                            -- ^ the strategy itself
                            transactStrategyQueue :: IORef (M.IntMap (DLL.DoubleLinkedList a))
                            -- ^ the transact queue
                          }

  newStrategyQueue s =
    liftIO $
    do r <- newIORef M.empty
       return $ TransactStrategyQueue s r

  strategyQueueNull q =
    liftIO $
    do m <- readIORef (transactStrategyQueue q)
       return $ M.null m

instance DequeueStrategy TransactQueueStrategy where

  strategyDequeue q =
    liftIO $ 
    do m <- readIORef (transactStrategyQueue q)
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
         modifyIORef (transactStrategyQueue q) $
         M.delete p
       return i

instance PriorityQueueStrategy TransactQueueStrategy Int where

  strategyEnqueueWithPriority q p i =
    liftIO $
    do m <- readIORef (transactStrategyQueue q)
       let xs = M.lookup p m
       case xs of
         Nothing ->
           do xs <- DLL.newList
              DLL.listAddLast xs i
              modifyIORef (transactStrategyQueue q) $
                M.insert p xs
         Just xs ->
           DLL.listAddLast xs i

-- | Try to delete the transact by the specified priority and satisfying to the provided predicate.
transactStrategyQueueDeleteBy :: StrategyQueue TransactQueueStrategy a
                                 -- ^ the queue
                                 -> Int
                                 -- ^ the transact priority
                                 -> (a -> Bool)
                                 -- ^ the predicate
                                 -> Event (Maybe a)
transactStrategyQueueDeleteBy q priority pred =
  liftIO $
  do m <- readIORef (transactStrategyQueue q)
     let xs = M.lookup priority m
     case xs of
       Nothing -> return Nothing
       Just xs ->
         do a <- DLL.listRemoveBy xs pred
            empty <- DLL.listNull xs
            when empty $
              modifyIORef (transactStrategyQueue q) $
              M.delete priority
            return a

-- | Test whether the queue contains a transact with the specified priority satisfying the provided predicate.
transactStrategyQueueContainsBy :: StrategyQueue TransactQueueStrategy a
                                   -- ^ the queue
                                   -> Int
                                   -- ^ the transact priority
                                   -> (a -> Bool)
                                   -- ^ the predicate
                                   -> Event (Maybe a)
transactStrategyQueueContainsBy q priority pred =
  liftIO $
  do m <- readIORef (transactStrategyQueue q)
     let xs = M.lookup priority m
     case xs of
       Nothing -> return Nothing
       Just xs -> DLL.listContainsBy xs pred
