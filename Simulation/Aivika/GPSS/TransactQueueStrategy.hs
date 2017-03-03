
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

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
data TransactQueueStrategy s = TransactQueueStrategy s

-- | An implementation of the 'QueueStrategy' class.
instance QueueStrategy (TransactQueueStrategy s) where

  -- | A queue used by the 'TransactQueueStrategy' strategy.
  data StrategyQueue (TransactQueueStrategy s) a =
    TransactStrategyQueue { transactStrategy :: TransactQueueStrategy s,
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

instance DequeueStrategy (TransactQueueStrategy FCFS) where

  strategyDequeue q =
    liftIO $ 
    do m <- readIORef (transactStrategyQueue q)
       let (k, xs) = M.findMin m
       i <- DLL.listFirst xs
       DLL.listRemoveFirst xs
       empty <- DLL.listNull xs
       when empty $
         modifyIORef (transactStrategyQueue q) $
         M.delete k
       return i

instance DequeueStrategy (TransactQueueStrategy LCFS) where

  strategyDequeue q =
    liftIO $ 
    do m <- readIORef (transactStrategyQueue q)
       let (k, xs) = M.findMin m
       i <- DLL.listLast xs
       DLL.listRemoveLast xs
       empty <- DLL.listNull xs
       when empty $
         modifyIORef (transactStrategyQueue q) $
         M.delete k
       return i

instance DequeueStrategy (TransactQueueStrategy s) => PriorityQueueStrategy (TransactQueueStrategy s) Int where

  {-# SPECIALISE instance PriorityQueueStrategy (TransactQueueStrategy FCFS) Int #-}
  {-# SPECIALISE instance PriorityQueueStrategy (TransactQueueStrategy LCFS) Int #-}
  
  strategyEnqueueWithPriority q priority i =
    liftIO $
    do m <- readIORef (transactStrategyQueue q)
       let k  = - priority
           xs = M.lookup k m
       case xs of
         Nothing ->
           do xs <- DLL.newList
              DLL.listAddLast xs i
              modifyIORef (transactStrategyQueue q) $
                M.insert k xs
         Just xs ->
           DLL.listAddLast xs i

instance DeletingQueueStrategy (TransactQueueStrategy FCFS) where

  strategyQueueDeleteBy q pred =
    liftIO $
    do m <- readIORef (transactStrategyQueue q)
       let loop [] = return Nothing
           loop ((k, xs): tail) =
             do a <- DLL.listRemoveBy xs pred
                case a of
                  Nothing -> loop tail
                  Just _  ->
                    do empty <- DLL.listNull xs
                       when empty $
                         modifyIORef (transactStrategyQueue q) $
                         M.delete k
                       return a
       loop (M.assocs m)

  strategyQueueContainsBy q pred =
    liftIO $
    do m <- readIORef (transactStrategyQueue q)
       let loop [] = return Nothing
           loop ((k, xs): tail) =
             do a <- DLL.listContainsBy xs pred
                case a of
                  Nothing -> loop tail
                  Just _  -> return a
       loop (M.assocs m)

-- | Try to delete the transact by the specified priority and satisfying to the provided predicate.
transactStrategyQueueDeleteBy :: StrategyQueue (TransactQueueStrategy s) a
                                 -- ^ the queue
                                 -> Int
                                 -- ^ the transact priority
                                 -> (a -> Bool)
                                 -- ^ the predicate
                                 -> Event (Maybe a)
transactStrategyQueueDeleteBy q priority pred =
  liftIO $
  do m <- readIORef (transactStrategyQueue q)
     let k  = - priority
         xs = M.lookup k m
     case xs of
       Nothing -> return Nothing
       Just xs ->
         do a <- DLL.listRemoveBy xs pred
            empty <- DLL.listNull xs
            when empty $
              modifyIORef (transactStrategyQueue q) $
              M.delete k
            return a

-- | Test whether the queue contains a transact with the specified priority satisfying the provided predicate.
transactStrategyQueueContainsBy :: StrategyQueue (TransactQueueStrategy s) a
                                   -- ^ the queue
                                   -> Int
                                   -- ^ the transact priority
                                   -> (a -> Bool)
                                   -- ^ the predicate
                                   -> Event (Maybe a)
transactStrategyQueueContainsBy q priority pred =
  liftIO $
  do m <- readIORef (transactStrategyQueue q)
     let k  = - priority
         xs = M.lookup k m
     case xs of
       Nothing -> return Nothing
       Just xs -> DLL.listContainsBy xs pred
