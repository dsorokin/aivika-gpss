
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Queue
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines a GPSS queue entity.
--
module Simulation.Aivika.Trans.GPSS.Queue
       (-- * Queue Types
        Queue,
        QueueEntry(..),
        -- * Creating Queue
        newQueue,
        -- * Queue Properties and Activities
        queueNull,
        queueCount,
        queueCountStats,
        enqueueCount,
        enqueueZeroEntryCount,
        dequeueCount,
        enqueueRate,
        dequeueRate,
        queueWaitTime,
        queueNonZeroEntryWaitTime,
        queueRate,
        -- * Dequeuing and Enqueuing
        enqueue,
        dequeue,
        -- * Derived Signals for Properties
        queueNullChanged,
        queueNullChanged_,
        queueCountChanged,
        queueCountChanged_,
        enqueueCountChanged,
        enqueueCountChanged_,
        enqueueZeroEntryCountChanged,
        enqueueZeroEntryCountChanged_,
        dequeueCountChanged,
        dequeueCountChanged_,
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

import Data.Monoid
import Data.Maybe
import Data.Hashable

import Control.Monad
import Control.Monad.Trans

import System.Mem.StableName

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Dynamics
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Process
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Statistics

import Simulation.Aivika.Trans.GPSS.Transact

-- | Represents the queue entity.
data Queue m =
  Queue { queueStableName :: StableName (Ref m Int),
          queueCountRef :: Ref m Int,
          queueCountStatsRef :: Ref m (TimingStats Int),
          enqueueCountRef :: Ref m Int,
          enqueueZeroEntryCountRef :: Ref m Int,
          queueWaitTimeRef :: Ref m (SamplingStats Double),
          queueNonZeroEntryWaitTimeRef :: Ref m (SamplingStats Double),
          enqueuedSource :: SignalSource m (),
          dequeuedSource :: SignalSource m ()
        }

-- | The information about queue entry.
data QueueEntry m =
  QueueEntry { entryQueue :: Queue m,
               -- ^ the entry queue
               entryEnqueueTime :: Double
               -- ^ the time of registering the queue entry
             } deriving Eq

instance MonadDES m => Eq (Queue m) where
  x == y = (queueCountRef x) == (queueCountRef y)

instance MonadDES m => Hashable (Queue m) where
  hashWithSalt salt x = hashWithSalt salt (queueStableName x)

-- | Create a new queue.
newQueue :: (MonadDES m, MonadIO (Event m)) => Event m (Queue m)
{-# INLINABLE newQueue #-}
newQueue =
  do t  <- liftDynamics time
     i  <- liftSimulation $ newRef 0
     is <- liftSimulation $ newRef $ returnTimingStats t 0
     e  <- liftSimulation $ newRef 0
     z  <- liftSimulation $ newRef 0 
     w  <- liftSimulation $ newRef mempty
     w2 <- liftSimulation $ newRef mempty
     s1 <- liftSimulation $ newSignalSource
     s2 <- liftSimulation $ newSignalSource
     sn <- liftIO $ makeStableName i
     return Queue { queueStableName = sn,
                    queueCountRef = i,
                    queueCountStatsRef = is,
                    enqueueCountRef = e,
                    enqueueZeroEntryCountRef = z,
                    queueWaitTimeRef = w,
                    queueNonZeroEntryWaitTimeRef = w2,
                    enqueuedSource = s1,
                    dequeuedSource = s2 }
  
-- | Test whether the queue is empty.
--
-- See also 'queueNullChanged' and 'queueNullChanged_'.
queueNull :: MonadDES m => Queue m -> Event m Bool
{-# INLINABLE queueNull #-}
queueNull q =
  Event $ \p ->
  do n <- invokeEvent p $ readRef (queueCountRef q)
     return (n == 0)
  
-- | Signal when the 'queueNull' property value has changed.
queueNullChanged :: MonadDES m => Queue m -> Signal m Bool
{-# INLINABLE queueNullChanged #-}
queueNullChanged q =
  mapSignalM (const $ queueNull q) (queueNullChanged_ q)
  
-- | Signal when the 'queueNull' property value has changed.
queueNullChanged_ :: MonadDES m => Queue m -> Signal m ()
{-# INLINABLE queueNullChanged_ #-}
queueNullChanged_ = queueCountChanged_

-- | Return the current queue size.
--
-- See also 'queueCountStats', 'queueCountChanged' and 'queueCountChanged_'.
queueCount :: MonadDES m => Queue m -> Event m Int
{-# INLINABLE queueCount #-}
queueCount q =
  Event $ \p -> invokeEvent p $ readRef (queueCountRef q)

-- | Return the queue size statistics.
queueCountStats :: MonadDES m => Queue m -> Event m (TimingStats Int)
{-# INLINABLE queueCountStats #-}
queueCountStats q =
  Event $ \p -> invokeEvent p $ readRef (queueCountStatsRef q)
  
-- | Signal when the 'queueCount' property value has changed.
queueCountChanged :: MonadDES m => Queue m -> Signal m Int
{-# INLINABLE queueCountChanged #-}
queueCountChanged q =
  mapSignalM (const $ queueCount q) (queueCountChanged_ q)
  
-- | Signal when the 'queueCount' property value has changed.
queueCountChanged_ :: MonadDES m => Queue m -> Signal m ()
{-# INLINABLE queueCountChanged_ #-}
queueCountChanged_ q =
  mapSignal (const ()) (enqueued q) <>
  mapSignal (const ()) (dequeued q)

-- | Return the total number of input items that were enqueued.
--
-- See also 'enqueueCountChanged' and 'enqueueCountChanged_'.
enqueueCount :: MonadDES m => Queue m -> Event m Int
{-# INLINABLE enqueueCount #-}
enqueueCount q =
  Event $ \p -> invokeEvent p $ readRef (enqueueCountRef q)
  
-- | Signal when the 'enqueueCount' property value has changed.
enqueueCountChanged :: MonadDES m => Queue m -> Signal m Int
{-# INLINABLE enqueueCountChanged #-}
enqueueCountChanged q =
  mapSignalM (const $ enqueueCount q) (enqueueCountChanged_ q)
  
-- | Signal when the 'enqueueCount' property value has changed.
enqueueCountChanged_ :: MonadDES m => Queue m -> Signal m ()
{-# INLINABLE enqueueCountChanged_ #-}
enqueueCountChanged_ q =
  mapSignal (const ()) (enqueued q)

-- | Return the total number of zero entry items.
--
-- See also 'enqueueZeroEntryCountChanged' and 'enqueueZeroEntryCountChanged_'.
enqueueZeroEntryCount :: MonadDES m => Queue m -> Event m Int
{-# INLINABLE enqueueZeroEntryCount #-}
enqueueZeroEntryCount q =
  Event $ \p -> invokeEvent p $ readRef (enqueueZeroEntryCountRef q)
  
-- | Signal when the 'enqueueZeroEntryCount' property value has changed.
enqueueZeroEntryCountChanged :: MonadDES m => Queue m -> Signal m Int
{-# INLINABLE enqueueZeroEntryCountChanged #-}
enqueueZeroEntryCountChanged q =
  mapSignalM (const $ enqueueZeroEntryCount q) (enqueueZeroEntryCountChanged_ q)
  
-- | Signal when the 'enqueueZeroEntryCount' property value has changed.
enqueueZeroEntryCountChanged_ :: MonadDES m => Queue m -> Signal m ()
{-# INLINABLE enqueueZeroEntryCountChanged_ #-}
enqueueZeroEntryCountChanged_ q =
  mapSignal (const ()) (dequeued q)

-- | Return the total number of input items that were dequeued.
--
-- See also 'dequeueCountChanged' and 'dequeueCountChanged_'.
dequeueCount :: MonadDES m => Queue m -> Event m Int
{-# INLINABLE dequeueCount #-}
dequeueCount q =
  Event $ \p ->
  do n1 <- invokeEvent p $ readRef (enqueueCountRef q)
     n2 <- invokeEvent p $ readRef (queueCountRef q)
     return (n1 - n2)
  
-- | Signal when the 'dequeueCount' property value has changed.
dequeueCountChanged :: MonadDES m => Queue m -> Signal m Int
{-# INLINABLE dequeueCountChanged #-}
dequeueCountChanged q =
  mapSignalM (const $ dequeueCount q) (dequeueCountChanged_ q)
  
-- | Signal when the 'dequeueCount' property value has changed.
dequeueCountChanged_ :: MonadDES m => Queue m -> Signal m ()
{-# INLINABLE dequeueCountChanged_ #-}
dequeueCountChanged_ q =
  mapSignal (const ()) (dequeued q)

-- | Return the rate of the input items that were enqueued: how many items
-- per time.
enqueueRate :: MonadDES m => Queue m -> Event m Double
{-# INLINABLE enqueueRate #-}
enqueueRate q =
  Event $ \p ->
  do x <- invokeEvent p $ readRef (enqueueCountRef q)
     let t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the rate of the requests for dequeueing the items: how many requests
-- per time.
dequeueRate :: MonadDES m => Queue m -> Event m Double
{-# INLINABLE dequeueRate #-}
dequeueRate q =
  Event $ \p ->
  do n1 <- invokeEvent p $ readRef (enqueueCountRef q)
     n2 <- invokeEvent p $ readRef (queueCountRef q)
     let x  = n1 - n2
         t0 = spcStartTime $ pointSpecs p
         t  = pointTime p
     return (fromIntegral x / (t - t0))
      
-- | Return the wait (or residence) time.
--
-- See also 'queueWaitTimeChanged' and 'queueWaitTimeChanged_'.
queueWaitTime :: MonadDES m => Queue m -> Event m (SamplingStats Double)
{-# INLINABLE queueWaitTime #-}
queueWaitTime q =
  Event $ \p -> invokeEvent p $ readRef (queueWaitTimeRef q)
      
-- | Signal when the 'queueWaitTime' property value has changed.
queueWaitTimeChanged :: MonadDES m => Queue m -> Signal m (SamplingStats Double)
{-# INLINABLE queueWaitTimeChanged #-}
queueWaitTimeChanged q =
  mapSignalM (const $ queueWaitTime q) (queueWaitTimeChanged_ q)
  
-- | Signal when the 'queueWaitTime' property value has changed.
queueWaitTimeChanged_ :: MonadDES m => Queue m -> Signal m ()
{-# INLINABLE queueWaitTimeChanged_ #-}
queueWaitTimeChanged_ q =
  mapSignal (const ()) (dequeued q)
      
-- | Return the wait (or residence) time excluding zero entries.
--
-- See also 'queueNonZeroEntryWaitTimeChanged' and 'queueNonZeroEntryWaitTimeChanged_'.
queueNonZeroEntryWaitTime :: MonadDES m => Queue m -> Event m (SamplingStats Double)
{-# INLINABLE queueNonZeroEntryWaitTime #-}
queueNonZeroEntryWaitTime q =
  Event $ \p -> invokeEvent p $ readRef (queueNonZeroEntryWaitTimeRef q)
      
-- | Signal when the 'queueNonZeroEntryWaitTime' property value has changed.
queueNonZeroEntryWaitTimeChanged :: MonadDES m => Queue m -> Signal m (SamplingStats Double)
{-# INLINABLE queueNonZeroEntryWaitTimeChanged #-}
queueNonZeroEntryWaitTimeChanged q =
  mapSignalM (const $ queueNonZeroEntryWaitTime q) (queueNonZeroEntryWaitTimeChanged_ q)
  
-- | Signal when the 'queueNonZeroEntryWaitTime' property value has changed.
queueNonZeroEntryWaitTimeChanged_ :: MonadDES m => Queue m -> Signal m ()
{-# INLINABLE queueNonZeroEntryWaitTimeChanged_ #-}
queueNonZeroEntryWaitTimeChanged_ q =
  mapSignal (const ()) (dequeued q)

-- | Return a long-term average queue rate calculated as
-- the average queue size divided by the average wait time.
--
-- See also 'queueRateChanged' and 'queueRateChanged_'.
queueRate :: MonadDES m => Queue m -> Event m Double
{-# INLINABLE queueRate #-}
queueRate q =
  Event $ \p ->
  do x <- invokeEvent p $ readRef (queueCountStatsRef q)
     y <- invokeEvent p $ readRef (queueWaitTimeRef q)
     return (timingStatsMean x / samplingStatsMean y) 
      
-- | Signal when the 'queueRate' property value has changed.
queueRateChanged :: MonadDES m => Queue m -> Signal m Double
{-# INLINABLE queueRateChanged #-}
queueRateChanged q =
  mapSignalM (const $ queueRate q) (queueRateChanged_ q)
      
-- | Signal when the 'queueRate' property value has changed.
queueRateChanged_ :: MonadDES m => Queue m -> Signal m ()
{-# INLINABLE queueRateChanged_ #-}
queueRateChanged_ q =
  mapSignal (const ()) (enqueued q) <>
  mapSignal (const ()) (dequeued q)

-- | Return a signal that notifies when enqueuing an item.
enqueued:: MonadDES m => Queue m -> Signal m ()
{-# INLINABLE enqueued #-}
enqueued q = publishSignal (enqueuedSource q)

-- | Return a signal that notifies when the dequeuing the item.
dequeued :: MonadDES m => Queue m -> Signal m ()
{-# INLINABLE dequeued #-}
dequeued q = publishSignal (dequeuedSource q)

-- | Enqueue the item.
enqueue :: MonadDES m
           => Queue m
           -- ^ the queue
           -> Transact m a
           -- ^ the item to be enqueued
           -> Event m ()
{-# INLINABLE enqueue #-}
enqueue q transact =
  Event $ \p ->
  do let t = pointTime p
         e = QueueEntry { entryQueue = q,
                          entryEnqueueTime = t }
     n <- invokeEvent p $ readRef (enqueueCountRef q)
     let n' = n + 1
     invokeEvent p $
       writeRef (enqueueCountRef q) n'
     c <- invokeEvent p $ readRef (queueCountRef q)
     let c' = c + 1
     invokeEvent p $
       writeRef (queueCountRef q) c'
     invokeEvent p $
       modifyRef (queueCountStatsRef q) (addTimingStats t c')
     invokeEvent p $
       registerTransactQueueEntry transact e
     invokeEvent p $
       triggerSignal (enqueuedSource q) ()

-- | Dequeue the item.
dequeue :: MonadDES m
           => Queue m
           -- ^ the queue
           -> Transact m a
           -- ^ the item to be dequeued
           -> Event m ()
{-# INLINABLE dequeue #-}
dequeue q transact =
  Event $ \p ->
  do e <- invokeEvent p $
          unregisterTransactQueueEntry transact q
     let t  = pointTime p
         t0 = entryEnqueueTime e
         dt = t - t0
     c <- invokeEvent p $ readRef (queueCountRef q)
     let c' = c - 1
     invokeEvent p $
       writeRef (queueCountRef q) c'
     invokeEvent p $
       modifyRef (queueCountStatsRef q) (addTimingStats t c')
     invokeEvent p $
       modifyRef (queueWaitTimeRef q) $
       addSamplingStats dt
     if t == t0
       then invokeEvent p $
            modifyRef (enqueueZeroEntryCountRef q) (+ 1)
       else invokeEvent p $
            modifyRef (queueNonZeroEntryWaitTimeRef q) $
            addSamplingStats dt
     invokeEvent p $
       triggerSignal (dequeuedSource q) ()

-- | Signal whenever any property of the queue changes.
--
-- The property must have the corresponded signal. There are also characteristics
-- similar to the properties but that have no signals. As a rule, such characteristics
-- already depend on the simulation time and therefore they may change at any
-- time point.
queueChanged_ :: MonadDES m => Queue m -> Signal m ()
{-# INLINABLE queueChanged_ #-}
queueChanged_ q =
  mapSignal (const ()) (enqueued q) <>
  mapSignal (const ()) (dequeued q)
