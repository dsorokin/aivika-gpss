
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
       (TransactQueueStrategy) where

import Control.Monad
import Control.Monad.Trans

import Data.IORef
import qualified Data.IntMap as M

import Simulation.Aivika
import qualified Simulation.Aivika.DoubleLinkedList as DLL

-- | The transact queue strategy.
data TransactQueueStrategy = TransactQueueStrategy

-- | An implementation of the 'QueueStrategy' class.
instance QueueStrategy TransactQueueStrategy where

  -- | A queue used by the 'TransactQueueStrategy' strategy.
  data StrategyQueue TransactQueueStrategy a =
    TransactStrategyQueue (IORef (M.IntMap (DLL.DoubleLinkedList a)))

  newStrategyQueue s =
    liftIO $
    do r <- newIORef M.empty
       return $ TransactStrategyQueue r

  strategyQueueNull (TransactStrategyQueue r) =
    liftIO $
    do m <- readIORef r
       return $ M.null m

instance DequeueStrategy TransactQueueStrategy where

  strategyDequeue (TransactStrategyQueue r) =
    liftIO $ 
    do m <- readIORef r
       let (p, xs) = M.findMax m
       i <- DLL.listFirst xs
       DLL.listRemoveFirst xs
       empty <- DLL.listNull xs
       when empty $
         modifyIORef r $
         M.delete p
       return i

instance PriorityQueueStrategy TransactQueueStrategy Int where

  strategyEnqueueWithPriority (TransactStrategyQueue r) p i =
    liftIO $
    do m <- readIORef r
       let xs = M.lookup p m
       case xs of
         Nothing ->
           do xs <- DLL.newList
              DLL.listAddLast xs i
         Just xs ->
           DLL.listAddLast xs i