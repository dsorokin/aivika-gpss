
-- |
-- Module     : Simulation.Aivika.GPSS.AssemblySet
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines a GPSS assembly set.
--
module Simulation.Aivika.GPSS.AssemblySet
       (-- * Types
        AssemblySet,
        -- * Creating Assembly Set
        newAssemblySet,
        -- * Functions
        assembleTransact,
        gatherTransacts,
        -- * Properties
        transactAssembling,
        transactGathering) where

import Data.IORef
import Data.Monoid
import Data.Maybe
import Data.Hashable

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika
import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Parameter
import Simulation.Aivika.Internal.Simulation

import Simulation.Aivika.GPSS.Transact
import Simulation.Aivika.GPSS.TransactQueueStrategy

-- | Represents an assembly set.
data AssemblySet =
  AssemblySet { assemblySetSequenceNo :: Int,
                assemblySetAssemblingTransact :: IORef (Maybe ProcessId),
                assemblySetAssemblingCounter :: IORef Int,
                assemblySetGatheringTransacts :: StrategyQueue (TransactQueueStrategy FCFS) ProcessId,
                assemblySetGatheringCounter :: IORef Int
              }

instance Eq AssemblySet where
  x == y = (assemblySetAssemblingTransact x) == (assemblySetAssemblingTransact y)

instance Hashable AssemblySet where
  hashWithSalt salt x = hashWithSalt salt (assemblySetSequenceNo x)

-- | Create a new assembly set.
newAssemblySet :: Simulation AssemblySet
newAssemblySet =
  Simulation $ \r ->
  do let g = runGenerator r
     sequenceNo <- generateSequenceNo g
     assemblingTransact <- newIORef Nothing
     assemblingCounter  <- newIORef 0
     gatheringTransacts <- invokeSimulation r $ newStrategyQueue (TransactQueueStrategy FCFS)
     gatheringCounter   <- newIORef 0
     return AssemblySet { assemblySetSequenceNo         = sequenceNo,
                          assemblySetAssemblingTransact = assemblingTransact,
                          assemblySetAssemblingCounter  = assemblingCounter,
                          assemblySetGatheringTransacts = gatheringTransacts,
                          assemblySetGatheringCounter   = gatheringCounter
                        }

-- | Assemble the transact by the specified number.
assembleTransact :: Transact a -> Int -> Process ()
assembleTransact t n =
  do (s, a) <-
       liftEvent $
       do s <- transactAssemblySet t
          a <- liftIO $ readIORef (assemblySetAssemblingCounter s)
          return (s, a)
     if a == 0
       then do let n' = n - 1
               when (n' < 0) $
                 throwProcess $
                 SimulationRetry
                 "The number of transacts must be positive: assembleTransact"
               if n' == 0
                 then return ()
                 else do liftEvent $
                           do pid <- requireTransactProcessId t
                              liftIO $ writeIORef (assemblySetAssemblingTransact s) (Just pid)
                              liftIO $ writeIORef (assemblySetAssemblingCounter s) $! n'
                         passivateProcess
       else do let a' = a - 1
               if a' == 0
                 then liftEvent $
                      do Just pid <- liftIO $ readIORef (assemblySetAssemblingTransact s)
                         liftIO $ writeIORef (assemblySetAssemblingTransact s) Nothing
                         liftIO $ writeIORef (assemblySetAssemblingCounter s) $! a'
                         reactivateProcess pid
                 else do liftIO $ writeIORef (assemblySetAssemblingCounter s) $! a'
                         cancelProcess

-- | Gather the transacts by the specified number.
gatherTransacts :: Transact a -> Int -> Process ()
gatherTransacts t n =
  do (s, a) <-
       liftEvent $
       do s <- transactAssemblySet t
          a <- liftIO $ readIORef (assemblySetGatheringCounter s)
          return (s, a)
     if a == 0
       then do let n' = n - 1
               when (n' < 0) $
                 throwProcess $
                 SimulationRetry
                 "The number of transacts must be positive: gatherTransacts"
               if n' == 0
                 then return ()
                 else do liftEvent $
                           do pid <- requireTransactProcessId t
                              strategyEnqueueWithPriority
                                (assemblySetGatheringTransacts s)
                                (transactPriority t)
                                pid
                              liftIO $ writeIORef (assemblySetGatheringCounter s) $! n'
                         passivateProcess
       else do let a' = a - 1
               if a' == 0
                 then liftEvent $
                      do let loop acc =
                               do f <- strategyQueueNull (assemblySetGatheringTransacts s)
                                  if f
                                    then return (reverse acc)
                                    else do x <- strategyDequeue (assemblySetGatheringTransacts s)
                                            loop (x: acc)
                         pids <- loop []
                         liftIO $ writeIORef (assemblySetGatheringCounter s) $! a'
                         forM_ pids reactivateProcess
                 else do liftEvent $
                           do pid <- requireTransactProcessId t
                              strategyEnqueueWithPriority
                                (assemblySetGatheringTransacts s)
                                (transactPriority t)
                                pid
                              liftIO $ writeIORef (assemblySetGatheringCounter s) $! a'
                         passivateProcess

-- | Test whether another transact is assembled for the corresponding assembly set.
transactAssembling :: Transact a -> Event Bool
transactAssembling t =
  do s <- transactAssemblySet t
     a <- liftIO $ readIORef (assemblySetAssemblingCounter s)
     return (a > 0)

-- | Test whether the transacts are gathered for the corresponding assembly set.
transactGathering :: Transact a -> Event Bool
transactGathering t =
  do s <- transactAssemblySet t
     a <- liftIO $ readIORef (assemblySetGatheringCounter s)
     return (a > 0)
