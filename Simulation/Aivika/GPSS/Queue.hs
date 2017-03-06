
-- |
-- Module     : Simulation.Aivika.GPSS.Queue
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines a GPSS queue entity.
--
module Simulation.Aivika.GPSS.Queue
       (-- * Queue Types
        Queue,
        QueueEntry(..),
        -- * Creating Queue
        newQueue,
        -- * Queue Properties and Activities
        queueNull,
        queueContent,
        queueContentStats,
        enqueueCount,
        enqueueZeroEntryCount,
        queueWaitTime,
        queueNonZeroEntryWaitTime,
        queueRate,
        -- * Dequeuing and Enqueuing
        enqueue,
        dequeue,
        -- * Statistics Reset
        resetQueue,
        -- * Derived Signals for Properties
        queueNullChanged,
        queueNullChanged_,
        queueContentChanged,
        queueContentChanged_,
        enqueueCountChanged,
        enqueueCountChanged_,
        enqueueZeroEntryCountChanged,
        enqueueZeroEntryCountChanged_,
        queueWaitTimeChanged,
        queueWaitTimeChanged_,
        queueNonZeroEntryWaitTimeChanged,
        queueNonZeroEntryWaitTimeChanged_,
        queueRateChanged,
        queueRateChanged_,
        -- * Basic Signals
        enqueued,
        dequeued,
        -- * Overall Signal
        queueChanged_) where

import Data.IORef
import Data.Monoid
import Data.Maybe
import Data.Hashable

import Control.Monad
import Control.Monad.Trans

import System.Mem.StableName

import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Dynamics
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Process
import Simulation.Aivika.Signal
import Simulation.Aivika.Statistics

import Simulation.Aivika.GPSS.Transact

-- | Represents the queue entity.
data Queue =
  Queue { queueStableName :: StableName (IORef Int),
          queueContentRef :: IORef Int,
          queueContentStatsRef :: IORef (TimingStats Int),
          enqueueCountRef :: IORef Int,
          enqueueZeroEntryCountRef :: IORef Int,
          queueWaitTimeRef :: IORef (SamplingStats Double),
          queueNonZeroEntryWaitTimeRef :: IORef (SamplingStats Double),
          enqueuedSource :: SignalSource (),
          dequeuedSource :: SignalSource ()
        }

-- | The information about queue entry.
data QueueEntry =
  QueueEntry { entryQueue :: Queue,
               -- ^ the entry queue
               entryEnqueueTime :: Double
               -- ^ the time of registering the queue entry
             } deriving Eq

instance Eq Queue where
  x == y = (queueContentRef x) == (queueContentRef y)

instance Hashable Queue where
  hashWithSalt salt x = hashWithSalt salt (queueStableName x)

-- | Create a new queue.
newQueue :: Event Queue  
newQueue =
  do t  <- liftDynamics time
     i  <- liftIO $ newIORef 0
     is <- liftIO $ newIORef $ returnTimingStats t 0
     e  <- liftIO $ newIORef 0
     z  <- liftIO $ newIORef 0 
     w  <- liftIO $ newIORef mempty
     w2 <- liftIO $ newIORef mempty
     s1 <- liftSimulation $ newSignalSource
     s2 <- liftSimulation $ newSignalSource
     sn <- liftIO $ makeStableName i
     return Queue { queueStableName = sn,
                    queueContentRef = i,
                    queueContentStatsRef = is,
                    enqueueCountRef = e,
                    enqueueZeroEntryCountRef = z,
                    queueWaitTimeRef = w,
                    queueNonZeroEntryWaitTimeRef = w2,
                    enqueuedSource = s1,
                    dequeuedSource = s2 }
  
-- | Test whether the queue is empty.
--
-- See also 'queueNullChanged' and 'queueNullChanged_'.
queueNull :: Queue -> Event Bool
queueNull q =
  Event $ \p ->
  do n <- readIORef (queueContentRef q)
     return (n == 0)
  
-- | Signal when the 'queueNull' property value has changed.
queueNullChanged :: Queue -> Signal Bool
queueNullChanged q =
  mapSignalM (const $ queueNull q) (queueNullChanged_ q)
  
-- | Signal when the 'queueNull' property value has changed.
queueNullChanged_ :: Queue -> Signal ()
queueNullChanged_ = queueContentChanged_

-- | Return the current queue content.
--
-- See also 'queueContentStats', 'queueContentChanged' and 'queueContentChanged_'.
queueContent :: Queue -> Event Int
queueContent q =
  Event $ \p -> readIORef (queueContentRef q)

-- | Return the queue content statistics.
queueContentStats :: Queue -> Event (TimingStats Int)
queueContentStats q =
  Event $ \p -> readIORef (queueContentStatsRef q)
  
-- | Signal when the 'queueContent' property value has changed.
queueContentChanged :: Queue -> Signal Int
queueContentChanged q =
  mapSignalM (const $ queueContent q) (queueContentChanged_ q)
  
-- | Signal when the 'queueContent' property value has changed.
queueContentChanged_ :: Queue -> Signal ()
queueContentChanged_ q =
  mapSignal (const ()) (enqueued q) <>
  mapSignal (const ()) (dequeued q)

-- | Return the total number of input items that were enqueued.
--
-- See also 'enqueueCountChanged' and 'enqueueCountChanged_'.
enqueueCount :: Queue -> Event Int
enqueueCount q =
  Event $ \p -> readIORef (enqueueCountRef q)
  
-- | Signal when the 'enqueueCount' property value has changed.
enqueueCountChanged :: Queue -> Signal Int
enqueueCountChanged q =
  mapSignalM (const $ enqueueCount q) (enqueueCountChanged_ q)
  
-- | Signal when the 'enqueueCount' property value has changed.
enqueueCountChanged_ :: Queue -> Signal ()
enqueueCountChanged_ q =
  mapSignal (const ()) (enqueued q)

-- | Return the total number of zero entry items.
--
-- See also 'enqueueZeroEntryCountChanged' and 'enqueueZeroEntryCountChanged_'.
enqueueZeroEntryCount :: Queue -> Event Int
enqueueZeroEntryCount q =
  Event $ \p -> readIORef (enqueueZeroEntryCountRef q)
  
-- | Signal when the 'enqueueZeroEntryCount' property value has changed.
enqueueZeroEntryCountChanged :: Queue -> Signal Int
enqueueZeroEntryCountChanged q =
  mapSignalM (const $ enqueueZeroEntryCount q) (enqueueZeroEntryCountChanged_ q)
  
-- | Signal when the 'enqueueZeroEntryCount' property value has changed.
enqueueZeroEntryCountChanged_ :: Queue -> Signal ()
enqueueZeroEntryCountChanged_ q =
  mapSignal (const ()) (dequeued q)

-- | Return the wait (or residence) time.
--
-- See also 'queueWaitTimeChanged' and 'queueWaitTimeChanged_'.
queueWaitTime :: Queue -> Event (SamplingStats Double)
queueWaitTime q =
  Event $ \p -> readIORef (queueWaitTimeRef q)
      
-- | Signal when the 'queueWaitTime' property value has changed.
queueWaitTimeChanged :: Queue -> Signal (SamplingStats Double)
queueWaitTimeChanged q =
  mapSignalM (const $ queueWaitTime q) (queueWaitTimeChanged_ q)
  
-- | Signal when the 'queueWaitTime' property value has changed.
queueWaitTimeChanged_ :: Queue -> Signal ()
queueWaitTimeChanged_ q =
  mapSignal (const ()) (dequeued q)
      
-- | Return the wait (or residence) time excluding zero entries.
--
-- See also 'queueNonZeroEntryWaitTimeChanged' and 'queueNonZeroEntryWaitTimeChanged_'.
queueNonZeroEntryWaitTime :: Queue -> Event (SamplingStats Double)
queueNonZeroEntryWaitTime q =
  Event $ \p -> readIORef (queueNonZeroEntryWaitTimeRef q)
      
-- | Signal when the 'queueNonZeroEntryWaitTime' property value has changed.
queueNonZeroEntryWaitTimeChanged :: Queue -> Signal (SamplingStats Double)
queueNonZeroEntryWaitTimeChanged q =
  mapSignalM (const $ queueNonZeroEntryWaitTime q) (queueNonZeroEntryWaitTimeChanged_ q)
  
-- | Signal when the 'queueNonZeroEntryWaitTime' property value has changed.
queueNonZeroEntryWaitTimeChanged_ :: Queue -> Signal ()
queueNonZeroEntryWaitTimeChanged_ q =
  mapSignal (const ()) (dequeued q)

-- | Return a long-term average queue rate calculated as
-- the average queue content divided by the average wait time.
--
-- See also 'queueRateChanged' and 'queueRateChanged_'.
queueRate :: Queue -> Event Double
queueRate q =
  Event $ \p ->
  do x <- readIORef (queueContentStatsRef q)
     y <- readIORef (queueWaitTimeRef q)
     return (timingStatsMean x / samplingStatsMean y) 
      
-- | Signal when the 'queueRate' property value has changed.
queueRateChanged :: Queue -> Signal Double
queueRateChanged q =
  mapSignalM (const $ queueRate q) (queueRateChanged_ q)
      
-- | Signal when the 'queueRate' property value has changed.
queueRateChanged_ :: Queue -> Signal ()
queueRateChanged_ q =
  mapSignal (const ()) (enqueued q) <>
  mapSignal (const ()) (dequeued q)

-- | Return a signal that notifies when enqueuing an item.
enqueued:: Queue -> Signal ()
enqueued q = publishSignal (enqueuedSource q)

-- | Return a signal that notifies when the dequeuing the item.
dequeued :: Queue -> Signal ()
dequeued q = publishSignal (dequeuedSource q)

-- | Enqueue the item.
enqueue :: Queue
           -- ^ the queue
           -> Transact a
           -- ^ the item to be enqueued
           -> Int
           -- ^ the content increment
           -> Event ()
enqueue q transact increment =
  Event $ \p ->
  do let t = pointTime p
         e = QueueEntry { entryQueue = q,
                          entryEnqueueTime = t }
     n <- readIORef (enqueueCountRef q)
     let n' = n + 1
     n' `seq` writeIORef (enqueueCountRef q) n'
     c <- readIORef (queueContentRef q)
     let c' = c + increment
     c' `seq` writeIORef (queueContentRef q) c'
     modifyIORef' (queueContentStatsRef q) (addTimingStats t c')
     invokeEvent p $
       registerTransactQueueEntry transact e
     invokeEvent p $
       triggerSignal (enqueuedSource q) ()

-- | Dequeue the item.
dequeue :: Queue
           -- ^ the queue
           -> Transact a
           -- ^ the item to be dequeued
           -> Int
           -- ^ the content decrement
           -> Event ()
dequeue q transact decrement =
  Event $ \p ->
  do e <- invokeEvent p $
          unregisterTransactQueueEntry transact q
     let t  = pointTime p
         t0 = entryEnqueueTime e
         dt = t - t0
     c <- readIORef (queueContentRef q)
     let c' = c - decrement
     c' `seq` writeIORef (queueContentRef q) c'
     modifyIORef' (queueContentStatsRef q) (addTimingStats t c')
     modifyIORef' (queueWaitTimeRef q) $
       addSamplingStats dt
     if t == t0
       then modifyIORef' (enqueueZeroEntryCountRef q) (+ 1)
       else modifyIORef' (queueNonZeroEntryWaitTimeRef q) $
            addSamplingStats dt
     invokeEvent p $
       triggerSignal (dequeuedSource q) ()

-- | Signal whenever any property of the queue changes.
--
-- The property must have the corresponded signal. There are also characteristics
-- similar to the properties but that have no signals. As a rule, such characteristics
-- already depend on the simulation time and therefore they may change at any
-- time point.
queueChanged_ :: Queue -> Signal ()
queueChanged_ q =
  mapSignal (const ()) (enqueued q) <>
  mapSignal (const ()) (dequeued q)

-- | Reset the statistics.
resetQueue :: Queue -> Event () 
resetQueue q =
  do t  <- liftDynamics time
     content <- liftIO $ readIORef (queueContentRef q)
     liftIO $ writeIORef (queueContentStatsRef q) $
       returnTimingStats t content
     liftIO $ writeIORef (enqueueCountRef q) 0
     liftIO $ writeIORef (enqueueZeroEntryCountRef q) 0
     liftIO $ writeIORef (queueWaitTimeRef q) mempty
     liftIO $ writeIORef (queueNonZeroEntryWaitTimeRef q) mempty
