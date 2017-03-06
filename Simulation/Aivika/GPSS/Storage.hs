
-- |
-- Module     : Simulation.Aivika.GPSS.Storage
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines the GPSS Storage entity.
--
module Simulation.Aivika.GPSS.Storage
       (-- * Storage Type
        Storage,
        -- * Creating Storage
        newStorage,
        -- * Storage Properties
        storageCapacity,
        storageEmpty,
        storageFull,
        storageContent,
        storageContentStats,
        storageUseCount,
        storageUsedContent,
        storageUtilisationCount,
        storageUtilisationCountStats,
        storageQueueCount,
        storageQueueCountStats,
        storageTotalWaitTime,
        storageWaitTime,
        storageAverageHoldingTime,
        -- * Entering-Leaving Storage
        enterStorage,
        leaveStorage,
        leaveStorageWithinEvent,
        -- * Statistics Reset
        resetStorage,
        -- * Signals
        storageContentChanged,
        storageContentChanged_,
        storageUseCountChanged,
        storageUseCountChanged_,
        storageUsedContentChanged,
        storageUsedContentChanged_,
        storageUtilisationCountChanged,
        storageUtilisationCountChanged_,
        storageQueueCountChanged,
        storageQueueCountChanged_,
        storageWaitTimeChanged,
        storageWaitTimeChanged_,
        storageChanged_) where

import Data.IORef
import Data.Monoid
import Data.Maybe

import Control.Monad
import Control.Monad.Trans
import Control.Exception

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Cont
import Simulation.Aivika.Internal.Process
import Simulation.Aivika.QueueStrategy
import Simulation.Aivika.Statistics
import Simulation.Aivika.Signal

import Simulation.Aivika.GPSS.Transact
import Simulation.Aivika.GPSS.TransactQueueStrategy

-- | Represents a GPSS Storage entity.
data Storage = 
  Storage { storageCapacity :: Int,
            -- ^ Return the storage capacity.
            storageContentRef :: IORef Int,
            storageContentStatsRef :: IORef (TimingStats Int),
            storageContentSource :: SignalSource Int,
            storageUseCountRef :: IORef Int,
            storageUseCountSource :: SignalSource Int,
            storageUsedContentRef :: IORef Int,
            storageUsedContentSource :: SignalSource Int,
            storageUtilisationCountRef :: IORef Int,
            storageUtilisationCountStatsRef :: IORef (TimingStats Int),
            storageUtilisationCountSource :: SignalSource Int,
            storageQueueCountRef :: IORef Int,
            storageQueueCountStatsRef :: IORef (TimingStats Int),
            storageQueueCountSource :: SignalSource Int,
            storageTotalWaitTimeRef :: IORef Double,
            storageWaitTimeRef :: IORef (SamplingStats Double),
            storageWaitTimeSource :: SignalSource (),
            storageDelayChain :: StrategyQueue (TransactQueueStrategy FCFS) StorageDelayedItem }

-- | Identifies an item that was delayed.
data StorageDelayedItem =
  StorageDelayedItem { delayedItemTime :: Double,
                       delayedItemDecrement :: Int,
                       delayedItemCont :: FrozenCont () }

instance Eq Storage where
  x == y = storageContentRef x == storageContentRef y  -- unique references

-- | Create a new storage by the specified capacity.
newStorage :: Int -> Event Storage
newStorage capacity =
  Event $ \p ->
  do let r = pointRun p
         t = pointTime p
     contentRef <- newIORef capacity
     contentStatsRef <- newIORef $ returnTimingStats t capacity
     contentSource <- invokeSimulation r newSignalSource
     useCountRef <- newIORef 0
     useCountSource <- invokeSimulation r newSignalSource
     usedContentRef <- newIORef 0
     usedContentSource <- invokeSimulation r newSignalSource
     utilCountRef <- newIORef 0
     utilCountStatsRef <- newIORef $ returnTimingStats t 0
     utilCountSource <- invokeSimulation r newSignalSource
     queueCountRef <- newIORef 0
     queueCountStatsRef <- newIORef $ returnTimingStats t 0
     queueCountSource <- invokeSimulation r newSignalSource
     totalWaitTimeRef <- newIORef 0
     waitTimeRef <- newIORef emptySamplingStats
     waitTimeSource <- invokeSimulation r newSignalSource
     delayChain <- invokeSimulation r $ newStrategyQueue (TransactQueueStrategy FCFS)
     return Storage { storageCapacity = capacity,
                      storageContentRef = contentRef,
                      storageContentStatsRef = contentStatsRef,
                      storageContentSource = contentSource,
                      storageUseCountRef = useCountRef,
                      storageUseCountSource = useCountSource,
                      storageUsedContentRef = usedContentRef,
                      storageUsedContentSource = usedContentSource,
                      storageUtilisationCountRef = utilCountRef,
                      storageUtilisationCountStatsRef = utilCountStatsRef,
                      storageUtilisationCountSource = utilCountSource,
                      storageQueueCountRef = queueCountRef,
                      storageQueueCountStatsRef = queueCountStatsRef,
                      storageQueueCountSource = queueCountSource,
                      storageTotalWaitTimeRef = totalWaitTimeRef,
                      storageWaitTimeRef = waitTimeRef,
                      storageWaitTimeSource = waitTimeSource,
                      storageDelayChain = delayChain }

-- | Whether the storage is empty, i.e. completely unused.
storageEmpty :: Storage -> Event Bool
storageEmpty r =
  Event $ \p ->
  do n <- readIORef (storageContentRef r)
     return (n == storageCapacity r)

-- | Whether the storage is full, i.e. completely used.
storageFull :: Storage -> Event Bool
storageFull r =
  Event $ \p ->
  do n <- readIORef (storageContentRef r)
     return (n == 0)

-- | Return the current storage content available for use.
storageContent :: Storage -> Event Int
storageContent r =
  Event $ \p -> readIORef (storageContentRef r)

-- | Return the statistics of the storage content available for use.
storageContentStats :: Storage -> Event (TimingStats Int)
storageContentStats r =
  Event $ \p -> readIORef (storageContentStatsRef r)

-- | Signal triggered when the 'storageContent' property changes.
storageContentChanged :: Storage -> Signal Int
storageContentChanged r =
  publishSignal $ storageContentSource r

-- | Signal triggered when the 'storageContent' property changes.
storageContentChanged_ :: Storage -> Signal ()
storageContentChanged_ r =
  mapSignal (const ()) $ storageContentChanged r

-- | Return the total use count of the storage.
storageUseCount :: Storage -> Event Int
storageUseCount r =
  Event $ \p -> readIORef (storageUseCountRef r)

-- | Signal triggered when the 'storageUseCount' property changes.
storageUseCountChanged :: Storage -> Signal Int
storageUseCountChanged r =
  publishSignal $ storageUseCountSource r

-- | Signal triggered when the 'storageUseCount' property changes.
storageUseCountChanged_ :: Storage -> Signal ()
storageUseCountChanged_ r =
  mapSignal (const ()) $ storageUseCountChanged r

-- | Return the total used content of the storage.
storageUsedContent :: Storage -> Event Int
storageUsedContent r =
  Event $ \p -> readIORef (storageUsedContentRef r)

-- | Signal triggered when the 'storageUsedContent' property changes.
storageUsedContentChanged :: Storage -> Signal Int
storageUsedContentChanged r =
  publishSignal $ storageUsedContentSource r

-- | Signal triggered when the 'storageUsedContent' property changes.
storageUsedContentChanged_ :: Storage -> Signal ()
storageUsedContentChanged_ r =
  mapSignal (const ()) $ storageUsedContentChanged r

-- | Return the current utilisation count of the storage.
storageUtilisationCount :: Storage -> Event Int
storageUtilisationCount r =
  Event $ \p -> readIORef (storageUtilisationCountRef r)

-- | Return the statistics for the utilisation count of the storage.
storageUtilisationCountStats :: Storage -> Event (TimingStats Int)
storageUtilisationCountStats r =
  Event $ \p -> readIORef (storageUtilisationCountStatsRef r)

-- | Signal triggered when the 'storageUtilisationCount' property changes.
storageUtilisationCountChanged :: Storage -> Signal Int
storageUtilisationCountChanged r =
  publishSignal $ storageUtilisationCountSource r

-- | Signal triggered when the 'storageUtilisationCount' property changes.
storageUtilisationCountChanged_ :: Storage -> Signal ()
storageUtilisationCountChanged_ r =
  mapSignal (const ()) $ storageUtilisationCountChanged r

-- | Return the current queue length of the storage.
storageQueueCount :: Storage -> Event Int
storageQueueCount r =
  Event $ \p -> readIORef (storageQueueCountRef r)

-- | Return the statistics for the queue length of the storage.
storageQueueCountStats :: Storage -> Event (TimingStats Int)
storageQueueCountStats r =
  Event $ \p -> readIORef (storageQueueCountStatsRef r)

-- | Signal triggered when the 'storageQueueCount' property changes.
storageQueueCountChanged :: Storage -> Signal Int
storageQueueCountChanged r =
  publishSignal $ storageQueueCountSource r

-- | Signal triggered when the 'storageQueueCount' property changes.
storageQueueCountChanged_ :: Storage -> Signal ()
storageQueueCountChanged_ r =
  mapSignal (const ()) $ storageQueueCountChanged r

-- | Return the total wait time of the storage.
storageTotalWaitTime :: Storage -> Event Double
storageTotalWaitTime r =
  Event $ \p -> readIORef (storageTotalWaitTimeRef r)

-- | Return the statistics for the wait time of the storage.
storageWaitTime :: Storage -> Event (SamplingStats Double)
storageWaitTime r =
  Event $ \p -> readIORef (storageWaitTimeRef r)

-- | Signal triggered when the 'storageTotalWaitTime' and 'storageWaitTime' properties change.
storageWaitTimeChanged :: Storage -> Signal (SamplingStats Double)
storageWaitTimeChanged r =
  mapSignalM (\() -> storageWaitTime r) $ storageWaitTimeChanged_ r

-- | Signal triggered when the 'storageTotalWaitTime' and 'storageWaitTime' properties change.
storageWaitTimeChanged_ :: Storage -> Signal ()
storageWaitTimeChanged_ r =
  publishSignal $ storageWaitTimeSource r

-- | Return the average holding time per unit.
storageAverageHoldingTime :: Storage -> Event Double
storageAverageHoldingTime r =
  Event $ \p ->
  do s <- readIORef (storageUtilisationCountStatsRef r)
     n <- readIORef (storageUtilisationCountRef r)
     m <- readIORef (storageUsedContentRef r)
     let t  = pointTime p
         s' = addTimingStats t n s
         k  = timingStatsSum s' / (fromRational $ toRational m)
     return k

-- | Enter the storage.
enterStorage :: Storage
                -- ^ the requested storage
                -> Transact a
                -- ^ a transact that makes the request
                -> Int
                -- ^ the content decrement
                -> Process ()
enterStorage r transact decrement =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do let t = pointTime p
     a <- readIORef (storageContentRef r)
     if a < decrement
       then do c <- invokeEvent p $
                    freezeContReentering c () $
                    invokeCont c $
                    invokeProcess pid $
                    enterStorage r transact decrement
               invokeEvent p $
                 strategyEnqueueWithPriority
                 (storageDelayChain r)
                 (transactPriority transact)
                 (StorageDelayedItem t decrement c)
               invokeEvent p $ updateStorageQueueCount r 1
       else do invokeEvent p $ updateStorageWaitTime r 0
               invokeEvent p $ updateStorageContent r (- decrement)
               invokeEvent p $ updateStorageUseCount r 1
               invokeEvent p $ updateStorageUsedContent r decrement
               invokeEvent p $ updateStorageUtilisationCount r decrement
               invokeEvent p $ resumeCont c ()

-- | Leave the storage.
leaveStorage :: Storage
                -- ^ the storage to leave
                -> Int
                -- ^ the content increment
                -> Process ()
leaveStorage r increment =
  Process $ \_ ->
  Cont $ \c ->
  Event $ \p ->
  do invokeEvent p $ leaveStorageWithinEvent r increment
     invokeEvent p $ resumeCont c ()

-- | Leave the storage.
leaveStorageWithinEvent :: Storage
                           -- ^ the storage to leave
                           -> Int
                           -- ^ the content increment
                           -> Event ()
leaveStorageWithinEvent r increment =
  Event $ \p ->
  do invokeEvent p $ updateStorageUtilisationCount r (- increment)
     invokeEvent p $ leaveStorage' r increment

-- | Leave the storage.
leaveStorage' :: Storage
                 -- ^ the storage to leave
                 -> Int
                 -- ^ the content increment
                 -> Event ()
leaveStorage' r increment =
  Event $ \p ->
  do let t = pointTime p
     a <- readIORef (storageContentRef r)
     let a' = a + increment
     when (a' > storageCapacity r) $
       throwIO $
       SimulationRetry $
       "The storage content cannot exceed the limited capacity: leaveStorage'"
     x <- invokeEvent p $
          strategyQueueDeleteBy
          (storageDelayChain r)
          (\i -> delayedItemDecrement i <= a')
     case x of
       Nothing -> invokeEvent p $ updateStorageContent r increment
       Just (StorageDelayedItem t0 decrement0 c0) ->
         do invokeEvent p $ updateStorageQueueCount r (-1)
            c <- invokeEvent p $ unfreezeCont c0
            case c of
              Nothing ->
                invokeEvent p $ leaveStorage' r increment
              Just c ->
                do invokeEvent p $ updateStorageContent r (increment - decrement0)
                   invokeEvent p $ updateStorageWaitTime r (t - t0)
                   invokeEvent p $ updateStorageUtilisationCount r decrement0
                   invokeEvent p $ updateStorageUseCount r 1
                   invokeEvent p $ updateStorageUsedContent r decrement0
                   invokeEvent p $ enqueueEvent t $ reenterCont c ()

-- | Signal triggered when one of the storage counters changes.
storageChanged_ :: Storage -> Signal ()
storageChanged_ r =
  storageContentChanged_ r <>
  storageUsedContentChanged_ r <>
  storageUtilisationCountChanged_ r <>
  storageQueueCountChanged_ r

-- | Update the storage content and its statistics.
updateStorageContent :: Storage -> Int -> Event ()
updateStorageContent r delta =
  Event $ \p ->
  do a <- readIORef (storageContentRef r)
     let a' = a + delta
     a' `seq` writeIORef (storageContentRef r) a'
     modifyIORef' (storageContentStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (storageContentSource r) a'

-- | Update the storage use count.
updateStorageUseCount :: Storage -> Int -> Event ()
updateStorageUseCount r delta =
  Event $ \p ->
  do a <- readIORef (storageUseCountRef r)
     let a' = a + delta
     a' `seq` writeIORef (storageUseCountRef r) a'
     invokeEvent p $
       triggerSignal (storageUseCountSource r) a'

-- | Update the storage used content.
updateStorageUsedContent :: Storage -> Int -> Event ()
updateStorageUsedContent r delta =
  Event $ \p ->
  do a <- readIORef (storageUsedContentRef r)
     let a' = a + delta
     a' `seq` writeIORef (storageUsedContentRef r) a'
     invokeEvent p $
       triggerSignal (storageUsedContentSource r) a'

-- | Update the storage queue length and its statistics.
updateStorageQueueCount :: Storage -> Int -> Event ()
updateStorageQueueCount r delta =
  Event $ \p ->
  do a <- readIORef (storageQueueCountRef r)
     let a' = a + delta
     a' `seq` writeIORef (storageQueueCountRef r) a'
     modifyIORef' (storageQueueCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (storageQueueCountSource r) a'

-- | Update the storage utilisation count and its statistics.
updateStorageUtilisationCount :: Storage -> Int -> Event ()
updateStorageUtilisationCount r delta =
  Event $ \p ->
  do a <- readIORef (storageUtilisationCountRef r)
     let a' = a + delta
     a' `seq` writeIORef (storageUtilisationCountRef r) a'
     modifyIORef' (storageUtilisationCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (storageUtilisationCountSource r) a'

-- | Update the storage wait time and its statistics.
updateStorageWaitTime :: Storage -> Double -> Event ()
updateStorageWaitTime r delta =
  Event $ \p ->
  do a <- readIORef (storageTotalWaitTimeRef r)
     let a' = a + delta
     a' `seq` writeIORef (storageTotalWaitTimeRef r) a'
     modifyIORef' (storageWaitTimeRef r) $
       addSamplingStats delta
     invokeEvent p $
       triggerSignal (storageWaitTimeSource r) ()

-- | Reset the statistics.
resetStorage :: Storage -> Event ()
resetStorage r =
  Event $ \p ->
  do let t = pointTime p
     content <- readIORef (storageContentRef r)
     writeIORef (storageContentStatsRef r) $
       returnTimingStats t content
     writeIORef (storageUseCountRef r) 0
     let usedContent = storageCapacity r - content
     writeIORef (storageUsedContentRef r) usedContent
     utilCount <- readIORef (storageUtilisationCountRef r)
     writeIORef (storageUtilisationCountStatsRef r) $
       returnTimingStats t utilCount
     queueCount <- readIORef (storageQueueCountRef r)
     writeIORef (storageQueueCountStatsRef r) $
       returnTimingStats t queueCount
     writeIORef (storageTotalWaitTimeRef r) 0
     writeIORef (storageWaitTimeRef r) emptySamplingStats
     invokeEvent p $
       triggerSignal (storageUseCountSource r) 0
     invokeEvent p $
       triggerSignal (storageUsedContentSource r) usedContent
     invokeEvent p $
       triggerSignal (storageUtilisationCountSource r) utilCount
     invokeEvent p $
       triggerSignal (storageWaitTimeSource r) ()
