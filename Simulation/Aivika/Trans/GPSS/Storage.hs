
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Storage
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines the GPSS Storage entity.
--
module Simulation.Aivika.Trans.GPSS.Storage
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

import Data.Monoid
import Data.Maybe

import Control.Monad
import Control.Monad.Trans
import Control.Exception

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Cont
import Simulation.Aivika.Trans.Internal.Process
import Simulation.Aivika.Trans.QueueStrategy
import Simulation.Aivika.Trans.Statistics
import Simulation.Aivika.Trans.Signal

import Simulation.Aivika.Trans.GPSS.Transact
import Simulation.Aivika.Trans.GPSS.TransactQueueStrategy

-- | Represents a GPSS Storage entity.
data Storage m = 
  Storage { storageCapacity :: Int,
            -- ^ Return the storage capacity.
            storageContentRef :: Ref m Int,
            storageContentStatsRef :: Ref m (TimingStats Int),
            storageContentSource :: SignalSource m Int,
            storageUseCountRef :: Ref m Int,
            storageUseCountSource :: SignalSource m Int,
            storageUsedContentRef :: Ref m Int,
            storageUsedContentSource :: SignalSource m Int,
            storageUtilisationCountRef :: Ref m Int,
            storageUtilisationCountStatsRef :: Ref m (TimingStats Int),
            storageUtilisationCountSource :: SignalSource m Int,
            storageQueueCountRef :: Ref m Int,
            storageQueueCountStatsRef :: Ref m (TimingStats Int),
            storageQueueCountSource :: SignalSource m Int,
            storageTotalWaitTimeRef :: Ref m Double,
            storageWaitTimeRef :: Ref m (SamplingStats Double),
            storageWaitTimeSource :: SignalSource m (),
            storageDelayChain :: StrategyQueue m (TransactQueueStrategy FCFS) (StorageDelayedItem m) }

-- | Identifies an item that was delayed.
data StorageDelayedItem m =
  StorageDelayedItem { delayedItemTime :: Double,
                       delayedItemDecrement :: Int,
                       delayedItemCont :: FrozenCont m () }

instance MonadDES m => Eq (Storage m) where

  {-# INLINABLE (==) #-}
  x == y = storageContentRef x == storageContentRef y  -- unique references

-- | Create a new storage by the specified capacity.
newStorage :: MonadDES m => Int -> Event m (Storage m)
{-# INLINABLE newStorage #-}
newStorage capacity =
  Event $ \p ->
  do let r = pointRun p
         t = pointTime p
     contentRef <- invokeSimulation r $ newRef capacity
     contentStatsRef <- invokeSimulation r $ newRef $ returnTimingStats t capacity
     contentSource <- invokeSimulation r newSignalSource
     useCountRef <- invokeSimulation r $ newRef 0
     useCountSource <- invokeSimulation r newSignalSource
     usedContentRef <- invokeSimulation r $ newRef 0
     usedContentSource <- invokeSimulation r newSignalSource
     utilCountRef <- invokeSimulation r $ newRef 0
     utilCountStatsRef <- invokeSimulation r $ newRef $ returnTimingStats t 0
     utilCountSource <- invokeSimulation r newSignalSource
     queueCountRef <- invokeSimulation r $ newRef 0
     queueCountStatsRef <- invokeSimulation r $ newRef $ returnTimingStats t 0
     queueCountSource <- invokeSimulation r newSignalSource
     totalWaitTimeRef <- invokeSimulation r $ newRef 0
     waitTimeRef <- invokeSimulation r $ newRef emptySamplingStats
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
storageEmpty :: MonadDES m => Storage m -> Event m Bool
{-# INLINABLE storageEmpty #-}
storageEmpty r =
  Event $ \p ->
  do n <- invokeEvent p $ readRef (storageContentRef r)
     return (n == storageCapacity r)

-- | Whether the storage is full, i.e. completely used.
storageFull :: MonadDES m => Storage m -> Event m Bool
{-# INLINABLE storageFull #-}
storageFull r =
  Event $ \p ->
  do n <- invokeEvent p $ readRef (storageContentRef r)
     return (n == 0)

-- | Return the current storage content available for use.
storageContent :: MonadDES m => Storage m -> Event m Int
{-# INLINABLE storageContent #-}
storageContent r =
  Event $ \p -> invokeEvent p $ readRef (storageContentRef r)

-- | Return the statistics of the storage content available for use.
storageContentStats :: MonadDES m => Storage m -> Event m (TimingStats Int)
{-# INLINABLE storageContentStats #-}
storageContentStats r =
  Event $ \p -> invokeEvent p $ readRef (storageContentStatsRef r)

-- | Signal triggered when the 'storageContent' property changes.
storageContentChanged :: MonadDES m => Storage m -> Signal m Int
{-# INLINABLE storageContentChanged #-}
storageContentChanged r =
  publishSignal $ storageContentSource r

-- | Signal triggered when the 'storageContent' property changes.
storageContentChanged_ :: MonadDES m => Storage m -> Signal m ()
{-# INLINABLE storageContentChanged_ #-}
storageContentChanged_ r =
  mapSignal (const ()) $ storageContentChanged r

-- | Return the total use count of the storage.
storageUseCount :: MonadDES m => Storage m -> Event m Int
{-# INLINABLE storageUseCount #-}
storageUseCount r =
  Event $ \p -> invokeEvent p $ readRef (storageUseCountRef r)

-- | Signal triggered when the 'storageUseCount' property changes.
storageUseCountChanged :: MonadDES m => Storage m -> Signal m Int
{-# INLINABLE storageUseCountChanged #-}
storageUseCountChanged r =
  publishSignal $ storageUseCountSource r

-- | Signal triggered when the 'storageUseCount' property changes.
storageUseCountChanged_ :: MonadDES m => Storage m -> Signal m ()
{-# INLINABLE storageUseCountChanged_ #-}
storageUseCountChanged_ r =
  mapSignal (const ()) $ storageUseCountChanged r

-- | Return the total used content of the storage.
storageUsedContent :: MonadDES m => Storage m -> Event m Int
{-# INLINABLE storageUsedContent #-}
storageUsedContent r =
  Event $ \p -> invokeEvent p $ readRef (storageUsedContentRef r)

-- | Signal triggered when the 'storageUsedContent' property changes.
storageUsedContentChanged :: MonadDES m => Storage m -> Signal m Int
{-# INLINABLE storageUsedContentChanged #-}
storageUsedContentChanged r =
  publishSignal $ storageUsedContentSource r

-- | Signal triggered when the 'storageUsedContent' property changes.
storageUsedContentChanged_ :: MonadDES m => Storage m -> Signal m ()
{-# INLINABLE storageUsedContentChanged_ #-}
storageUsedContentChanged_ r =
  mapSignal (const ()) $ storageUsedContentChanged r

-- | Return the current utilisation count of the storage.
storageUtilisationCount :: MonadDES m => Storage m -> Event m Int
{-# INLINABLE storageUtilisationCount #-}
storageUtilisationCount r =
  Event $ \p -> invokeEvent p $ readRef (storageUtilisationCountRef r)

-- | Return the statistics for the utilisation count of the storage.
storageUtilisationCountStats :: MonadDES m => Storage m -> Event m (TimingStats Int)
{-# INLINABLE storageUtilisationCountStats #-}
storageUtilisationCountStats r =
  Event $ \p -> invokeEvent p $ readRef (storageUtilisationCountStatsRef r)

-- | Signal triggered when the 'storageUtilisationCount' property changes.
storageUtilisationCountChanged :: MonadDES m => Storage m -> Signal m Int
{-# INLINABLE storageUtilisationCountChanged #-}
storageUtilisationCountChanged r =
  publishSignal $ storageUtilisationCountSource r

-- | Signal triggered when the 'storageUtilisationCount' property changes.
storageUtilisationCountChanged_ :: MonadDES m => Storage m -> Signal m ()
{-# INLINABLE storageUtilisationCountChanged_ #-}
storageUtilisationCountChanged_ r =
  mapSignal (const ()) $ storageUtilisationCountChanged r

-- | Return the current queue length of the storage.
storageQueueCount :: MonadDES m => Storage m -> Event m Int
{-# INLINABLE storageQueueCount #-}
storageQueueCount r =
  Event $ \p -> invokeEvent p $ readRef (storageQueueCountRef r)

-- | Return the statistics for the queue length of the storage.
storageQueueCountStats :: MonadDES m => Storage m -> Event m (TimingStats Int)
{-# INLINABLE storageQueueCountStats #-}
storageQueueCountStats r =
  Event $ \p -> invokeEvent p $ readRef (storageQueueCountStatsRef r)

-- | Signal triggered when the 'storageQueueCount' property changes.
storageQueueCountChanged :: MonadDES m => Storage m -> Signal m Int
{-# INLINABLE storageQueueCountChanged #-}
storageQueueCountChanged r =
  publishSignal $ storageQueueCountSource r

-- | Signal triggered when the 'storageQueueCount' property changes.
storageQueueCountChanged_ :: MonadDES m => Storage m -> Signal m ()
{-# INLINABLE storageQueueCountChanged_ #-}
storageQueueCountChanged_ r =
  mapSignal (const ()) $ storageQueueCountChanged r

-- | Return the total wait time of the storage.
storageTotalWaitTime :: MonadDES m => Storage m -> Event m Double
{-# INLINABLE storageTotalWaitTime #-}
storageTotalWaitTime r =
  Event $ \p -> invokeEvent p $ readRef (storageTotalWaitTimeRef r)

-- | Return the statistics for the wait time of the storage.
storageWaitTime :: MonadDES m => Storage m -> Event m (SamplingStats Double)
{-# INLINABLE storageWaitTime #-}
storageWaitTime r =
  Event $ \p -> invokeEvent p $ readRef (storageWaitTimeRef r)

-- | Signal triggered when the 'storageTotalWaitTime' and 'storageWaitTime' properties change.
storageWaitTimeChanged :: MonadDES m => Storage m -> Signal m (SamplingStats Double)
{-# INLINABLE storageWaitTimeChanged #-}
storageWaitTimeChanged r =
  mapSignalM (\() -> storageWaitTime r) $ storageWaitTimeChanged_ r

-- | Signal triggered when the 'storageTotalWaitTime' and 'storageWaitTime' properties change.
storageWaitTimeChanged_ :: MonadDES m => Storage m -> Signal m ()
{-# INLINABLE storageWaitTimeChanged_ #-}
storageWaitTimeChanged_ r =
  publishSignal $ storageWaitTimeSource r

-- | Return the average holding time per unit.
storageAverageHoldingTime :: MonadDES m => Storage m -> Event m Double
{-# INLINABLE storageAverageHoldingTime #-}
storageAverageHoldingTime r =
  Event $ \p ->
  do s <- invokeEvent p $ readRef (storageUtilisationCountStatsRef r)
     n <- invokeEvent p $ readRef (storageUtilisationCountRef r)
     m <- invokeEvent p $ readRef (storageUsedContentRef r)
     let t  = pointTime p
         s' = addTimingStats t n s
         k  = timingStatsSum s' / (fromRational $ toRational m)
     return k

-- | Enter the storage.
enterStorage :: MonadDES m
                => Storage m
                -- ^ the requested storage
                -> Transact m a
                -- ^ a transact that makes the request
                -> Int
                -- ^ the content decrement
                -> Process m ()
{-# INLINABLE enterStorage #-}
enterStorage r transact decrement =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do let t = pointTime p
     f <- invokeEvent p $ strategyQueueNull (storageDelayChain r)
     if f
       then invokeEvent p $
            invokeCont c $
            invokeProcess pid $
            enterStorage' r transact decrement
       else do c <- invokeEvent p $
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

-- | Enter the storage.
enterStorage' :: MonadDES m
                 => Storage m
                 -- ^ the requested storage
                 -> Transact m a
                 -- ^ a transact that makes the request
                 -> Int
                 -- ^ the content decrement
                 -> Process m ()
{-# INLINABLE enterStorage' #-}
enterStorage' r transact decrement =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do let t = pointTime p
     a <- invokeEvent p $ readRef (storageContentRef r)
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
leaveStorage :: MonadDES m
                => Storage m
                -- ^ the storage to leave
                -> Int
                -- ^ the content increment
                -> Process m ()
{-# INLINABLE leaveStorage #-}
leaveStorage r increment =
  Process $ \_ ->
  Cont $ \c ->
  Event $ \p ->
  do invokeEvent p $ leaveStorageWithinEvent r increment
     invokeEvent p $ resumeCont c ()

-- | Leave the storage.
leaveStorageWithinEvent :: MonadDES m
                           => Storage m
                           -- ^ the storage to leave
                           -> Int
                           -- ^ the content increment
                           -> Event m ()
{-# INLINABLE leaveStorageWithinEvent #-}
leaveStorageWithinEvent r increment =
  Event $ \p ->
  do let t = pointTime p
     invokeEvent p $ updateStorageUtilisationCount r (- increment)
     invokeEvent p $ updateStorageContent r increment
     invokeEvent p $ enqueueEvent t $ tryEnterStorage r

-- | Try to enter the storage.
tryEnterStorage :: MonadDES m => Storage m -> Event m ()
{-# INLINABLE tryEnterStorage #-}
tryEnterStorage r =
  Event $ \p ->
  do let t = pointTime p
     a <- invokeEvent p $ readRef (storageContentRef r)
     if a > 0
       then invokeEvent p $ letEnterStorage r
       else return ()

-- | Let enter the storage.
letEnterStorage :: MonadDES m => Storage m -> Event m ()
{-# INLINABLE letEnterStorage #-}
letEnterStorage r =
  Event $ \p ->
  do let t = pointTime p
     a <- invokeEvent p $ readRef (storageContentRef r)
     when (a > storageCapacity r) $
       throwComp $
       SimulationRetry $
       "The storage content cannot exceed the limited capacity: leaveStorage'"
     x <- invokeEvent p $
          strategyQueueDeleteBy
          (storageDelayChain r)
          (\i -> delayedItemDecrement i <= a)
     case x of
       Nothing -> return ()
       Just (StorageDelayedItem t0 decrement0 c0) ->
         do invokeEvent p $ updateStorageQueueCount r (-1)
            c <- invokeEvent p $ unfreezeCont c0
            case c of
              Nothing ->
                invokeEvent p $ letEnterStorage r
              Just c ->
                do invokeEvent p $ updateStorageContent r (- decrement0)
                   invokeEvent p $ updateStorageWaitTime r (t - t0)
                   invokeEvent p $ updateStorageUtilisationCount r decrement0
                   invokeEvent p $ updateStorageUseCount r 1
                   invokeEvent p $ updateStorageUsedContent r decrement0
                   invokeEvent p $ enqueueEvent t $ reenterCont c ()

-- | Signal triggered when one of the storage counters changes.
storageChanged_ :: MonadDES m => Storage m -> Signal m ()
{-# INLINABLE storageChanged_ #-}
storageChanged_ r =
  storageContentChanged_ r <>
  storageUsedContentChanged_ r <>
  storageUtilisationCountChanged_ r <>
  storageQueueCountChanged_ r

-- | Update the storage content and its statistics.
updateStorageContent :: MonadDES m => Storage m -> Int -> Event m ()
{-# INLINABLE updateStorageContent #-}
updateStorageContent r delta =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (storageContentRef r)
     let a' = a + delta
     invokeEvent p $
       writeRef (storageContentRef r) a'
     invokeEvent p $
       modifyRef (storageContentStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (storageContentSource r) a'

-- | Update the storage use count.
updateStorageUseCount :: MonadDES m => Storage m -> Int -> Event m ()
{-# INLINABLE updateStorageUseCount #-}
updateStorageUseCount r delta =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (storageUseCountRef r)
     let a' = a + delta
     invokeEvent p $
       writeRef (storageUseCountRef r) a'
     invokeEvent p $
       triggerSignal (storageUseCountSource r) a'

-- | Update the storage used content.
updateStorageUsedContent :: MonadDES m => Storage m -> Int -> Event m ()
{-# INLINABLE updateStorageUsedContent #-}
updateStorageUsedContent r delta =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (storageUsedContentRef r)
     let a' = a + delta
     invokeEvent p $
       writeRef (storageUsedContentRef r) a'
     invokeEvent p $
       triggerSignal (storageUsedContentSource r) a'

-- | Update the storage queue length and its statistics.
updateStorageQueueCount :: MonadDES m => Storage m -> Int -> Event m ()
{-# INLINABLE updateStorageQueueCount #-}
updateStorageQueueCount r delta =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (storageQueueCountRef r)
     let a' = a + delta
     invokeEvent p $
       writeRef (storageQueueCountRef r) a'
     invokeEvent p $
       modifyRef (storageQueueCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (storageQueueCountSource r) a'

-- | Update the storage utilisation count and its statistics.
updateStorageUtilisationCount :: MonadDES m => Storage m -> Int -> Event m ()
{-# INLINABLE updateStorageUtilisationCount #-}
updateStorageUtilisationCount r delta =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (storageUtilisationCountRef r)
     let a' = a + delta
     invokeEvent p $
       writeRef (storageUtilisationCountRef r) a'
     invokeEvent p $
       modifyRef (storageUtilisationCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (storageUtilisationCountSource r) a'

-- | Update the storage wait time and its statistics.
updateStorageWaitTime :: MonadDES m => Storage m -> Double -> Event m ()
{-# INLINABLE updateStorageWaitTime #-}
updateStorageWaitTime r delta =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (storageTotalWaitTimeRef r)
     let a' = a + delta
     invokeEvent p $
       writeRef (storageTotalWaitTimeRef r) a'
     invokeEvent p $
       modifyRef (storageWaitTimeRef r) $
       addSamplingStats delta
     invokeEvent p $
       triggerSignal (storageWaitTimeSource r) ()

-- | Reset the statistics.
resetStorage :: MonadDES m => Storage m -> Event m ()
{-# INLINABLE resetStorage #-}
resetStorage r =
  Event $ \p ->
  do let t = pointTime p
     content <- invokeEvent p $ readRef (storageContentRef r)
     invokeEvent p $ writeRef (storageContentStatsRef r) $
       returnTimingStats t content
     invokeEvent p $ writeRef (storageUseCountRef r) 0
     let usedContent = storageCapacity r - content
     invokeEvent p $ writeRef (storageUsedContentRef r) usedContent
     utilCount <- invokeEvent p $ readRef (storageUtilisationCountRef r)
     invokeEvent p $ writeRef (storageUtilisationCountStatsRef r) $
       returnTimingStats t utilCount
     queueCount <- invokeEvent p $ readRef (storageQueueCountRef r)
     invokeEvent p $ writeRef (storageQueueCountStatsRef r) $
       returnTimingStats t queueCount
     invokeEvent p $ writeRef (storageTotalWaitTimeRef r) 0
     invokeEvent p $ writeRef (storageWaitTimeRef r) emptySamplingStats
     invokeEvent p $
       triggerSignal (storageUseCountSource r) 0
     invokeEvent p $
       triggerSignal (storageUsedContentSource r) usedContent
     invokeEvent p $
       triggerSignal (storageUtilisationCountSource r) utilCount
     invokeEvent p $
       triggerSignal (storageWaitTimeSource r) ()
