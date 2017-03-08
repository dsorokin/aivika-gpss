
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.AssemblySet
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines a GPSS assembly set.
--
module Simulation.Aivika.Trans.GPSS.AssemblySet
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

import Data.Monoid
import Data.Maybe
import Data.Hashable

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Parameter
import Simulation.Aivika.Trans.Internal.Simulation

import Simulation.Aivika.Trans.GPSS.Transact
import Simulation.Aivika.Trans.GPSS.TransactQueueStrategy

-- | Represents an assembly set.
data AssemblySet m =
  AssemblySet { assemblySetSequenceNo :: Int,
                assemblySetAssemblingTransact :: Ref m (Maybe (ProcessId m)),
                assemblySetAssemblingCounter :: Ref m Int,
                assemblySetGatheringTransacts :: StrategyQueue m (TransactQueueStrategy FCFS) (ProcessId m),
                assemblySetGatheringCounter :: Ref m Int
              }

instance MonadDES m => Eq (AssemblySet m) where

  {-# INLINABLE (==) #-}
  x == y = (assemblySetAssemblingTransact x) == (assemblySetAssemblingTransact y)

instance Hashable (AssemblySet m) where
  hashWithSalt salt x = hashWithSalt salt (assemblySetSequenceNo x)

-- | Create a new assembly set.
newAssemblySet :: MonadDES m => Simulation m (AssemblySet m)
{-# INLINABLE newAssemblySet #-}
newAssemblySet =
  Simulation $ \r ->
  do let g = runGenerator r
     sequenceNo <- generateSequenceNo g
     assemblingTransact <- invokeSimulation r $ newRef Nothing
     assemblingCounter  <- invokeSimulation r $ newRef 0
     gatheringTransacts <- invokeSimulation r $ newStrategyQueue (TransactQueueStrategy FCFS)
     gatheringCounter   <- invokeSimulation r $ newRef 0
     return AssemblySet { assemblySetSequenceNo         = sequenceNo,
                          assemblySetAssemblingTransact = assemblingTransact,
                          assemblySetAssemblingCounter  = assemblingCounter,
                          assemblySetGatheringTransacts = gatheringTransacts,
                          assemblySetGatheringCounter   = gatheringCounter
                        }

-- | Assemble the transact by the specified number.
assembleTransact :: MonadDES m => Transact m a -> Int -> Process m ()
{-# INLINABLE assembleTransact #-}
assembleTransact t n =
  do (s, a) <-
       liftEvent $
       do s <- transactAssemblySet t
          a <- readRef (assemblySetAssemblingCounter s)
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
                              writeRef (assemblySetAssemblingTransact s) (Just pid)
                              writeRef (assemblySetAssemblingCounter s) $! n'
                         passivateProcess
       else do let a' = a - 1
               if a' == 0
                 then do liftEvent $
                           do Just pid <- readRef (assemblySetAssemblingTransact s)
                              writeRef (assemblySetAssemblingTransact s) Nothing
                              writeRef (assemblySetAssemblingCounter s) $! a'
                              reactivateProcessImmediately pid
                         cancelProcess
                 else do liftEvent $ writeRef (assemblySetAssemblingCounter s) $! a'
                         cancelProcess

-- | Gather the transacts by the specified number.
gatherTransacts :: MonadDES m => Transact m a -> Int -> Process m ()
{-# INLINABLE gatherTransacts #-}
gatherTransacts t n =
  do (s, a) <-
       liftEvent $
       do s <- transactAssemblySet t
          a <- readRef (assemblySetGatheringCounter s)
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
                              writeRef (assemblySetGatheringCounter s) $! n'
                         passivateProcess
       else do let a' = a - 1
               liftEvent $
                 do pid <- requireTransactProcessId t
                    strategyEnqueueWithPriority
                      (assemblySetGatheringTransacts s)
                      (transactPriority t)
                      pid
                    writeRef (assemblySetGatheringCounter s) $! a'
               if a' == 0
                 then liftEvent $
                      do let loop acc =
                               do f <- strategyQueueNull (assemblySetGatheringTransacts s)
                                  if f
                                    then return (reverse acc)
                                    else do x <- strategyDequeue (assemblySetGatheringTransacts s)
                                            loop (x: acc)
                             act [] = return ()
                             act (pid: pids') =
                               yieldEvent $
                               do reactivateProcessImmediately pid
                                  act pids'
                         pids <- loop []
                         act pids
                 else return ()
               passivateProcess

-- | Test whether another transact is assembled for the corresponding assembly set.
transactAssembling :: MonadDES m => Transact m a -> Event m Bool
{-# INLINABLE transactAssembling #-}
transactAssembling t =
  do s <- transactAssemblySet t
     a <- readRef (assemblySetAssemblingCounter s)
     return (a > 0)

-- | Test whether the transacts are gathered for the corresponding assembly set.
transactGathering :: MonadDES m => Transact m a -> Event m Bool
{-# INLINABLE transactGathering #-}
transactGathering t =
  do s <- transactAssemblySet t
     a <- readRef (assemblySetGatheringCounter s)
     return (a > 0)
