
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Transact
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines a GPSS transact.
--
module Simulation.Aivika.Trans.GPSS.Transact
       (Transact,
        transactValue,
        transactArrivalDelay,
        transactArrivalTime,
        transactPriority,
        newTransact,
        takeTransact,
        releaseTransact,
        transactPreemptionBegin,
        transactPreemptionEnd,
        requireTransactProcessId,
        transferTransact,
        registerTransactQueueEntry,
        unregisterTransactQueueEntry) where

import Control.Monad
import Control.Monad.Trans
import Control.Exception

import qualified Data.HashMap.Lazy as HM

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Simulation
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Cont
import Simulation.Aivika.Trans.Internal.Process

import {-# SOURCE #-} Simulation.Aivika.Trans.GPSS.Queue

-- | Represents a GPSS transact.
data Transact m a =
  Transact { transactValue :: a,
             -- ^ The data of the transact.
             transactArrivalDelay :: Maybe Double,
             -- ^ The delay between the transacts generated.
             transactArrivalTime :: Double,
             -- ^ The time at which the transact was generated.
             transactPriority :: Int,
             -- ^ The transact priority.
             transactPreemptionCountRef :: Ref m Int,
             -- ^ How many times the transact is preempted.
             transactProcessIdRef :: Ref m (Maybe (ProcessId m)),
             -- ^ An identifier of the process that handles the transact at present
             transactProcessContRef :: Ref m (Maybe (FrozenCont m ())),
             -- ^ A continuation of the process that tried to handle the transact.
             transactQueueEntryRef :: Ref m (HM.HashMap (Queue m) (QueueEntry m))
             -- ^ The queue entries registered by the the transact.
           }

instance MonadDES m => Eq (Transact m a) where

  {-# INLINABLE (==) #-}
  x == y = (transactProcessIdRef x) == (transactProcessIdRef y)

-- | Create a new transact.
newTransact :: MonadDES m
               => Arrival a
               -- ^ the arrival data
               -> Int
               -- ^ the transact priority
               -> Simulation m (Transact m a)
{-# INLINABLE newTransact #-}
newTransact a priority =
  Simulation $ \r ->
  do r0 <- invokeSimulation r $ newRef 0
     r1 <- invokeSimulation r $ newRef Nothing
     r2 <- invokeSimulation r $ newRef Nothing
     r3 <- invokeSimulation r $ newRef HM.empty
     return Transact { transactValue = arrivalValue a,
                       transactArrivalDelay = arrivalDelay a,
                       transactArrivalTime = arrivalTime a,
                       transactPriority = priority,
                       transactPreemptionCountRef = r0,
                       transactProcessIdRef = r1,
                       transactProcessContRef = r2,
                       transactQueueEntryRef = r3
                     }

-- | Take the transact.
takeTransact :: MonadDES m => Transact m a -> Process m ()
{-# INLINABLE takeTransact #-}
takeTransact t =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do pid0 <- invokeEvent p $ readRef (transactProcessIdRef t)
     case pid0 of
       Just pid0 ->
         throwComp $
         SimulationRetry
         "The transact is acquired by another process: takeTransact"
       Nothing   ->
         do invokeEvent p $ writeRef (transactProcessIdRef t) (Just pid)
            n <- invokeEvent p $ readRef (transactPreemptionCountRef t)
            if n == 0
              then invokeEvent p $ resumeCont c ()
              else do c <- invokeEvent p $
                           freezeContReentering c () $
                           invokeCont c $
                           invokeProcess pid $
                           takeTransact t
                      invokeEvent p $
                        writeRef (transactProcessContRef t) (Just c)
                      forM_ [1 .. n] $ \_ ->
                        invokeEvent p $
                        processPreemptionBegin pid

-- | Release the transact.
releaseTransact :: MonadDES m => Transact m a -> Process m ()
{-# INLINABLE releaseTransact #-}
releaseTransact t =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do pid0 <- invokeEvent p $ readRef (transactProcessIdRef t)
     case pid0 of
       Nothing ->
         throwComp $
         SimulationRetry
         "The transact is not acquired by any process: releaseTransact"
       Just pid0 | pid0 /= pid ->
         throwComp $
         SimulationRetry
         "The transact is acquired by another process: releaseTransact"
       Just pid0 ->
         do invokeEvent p $ writeRef (transactProcessIdRef t) Nothing
            invokeEvent p $ writeRef (transactProcessContRef t) Nothing
            invokeEvent p $ resumeCont c ()

-- | Preempt the computation that handles the transact.
transactPreemptionBegin :: MonadDES m => Transact m a -> Event m ()
{-# INLINABLE transactPreemptionBegin #-}
transactPreemptionBegin t =
  Event $ \p ->
  do n <- invokeEvent p $ readRef (transactPreemptionCountRef t)
     let n' = n + 1
     n' `seq` invokeEvent p $ writeRef (transactPreemptionCountRef t) n'
     pid <- invokeEvent p $ readRef (transactProcessIdRef t)
     case pid of
       Nothing  -> return ()
       Just pid -> invokeEvent p $ processPreemptionBegin pid

-- | Proceed with the computation after the transact was preempted earlier.
transactPreemptionEnd :: MonadDES m => Transact m a -> Event m ()
{-# INLINABLE transactPreemptionEnd #-}
transactPreemptionEnd t =
  Event $ \p ->
  do n <- invokeEvent p $ readRef (transactPreemptionCountRef t)
     let n' = n - 1
     unless (n' >= 0) $
       throwComp $
       SimulationRetry
       "The transact preemption count cannot be negative: transactPreemptionEnd"
     n' `seq` invokeEvent p $ writeRef (transactPreemptionCountRef t) n'
     pid <- invokeEvent p $ readRef (transactProcessIdRef t)
     case pid of
       Nothing  -> return ()
       Just pid ->
         do invokeEvent p $ processPreemptionEnd pid
            c <- invokeEvent p $ readRef (transactProcessContRef t)
            case c of
              Nothing -> return ()
              Just c  ->
                do invokeEvent p $ writeRef (transactProcessContRef t) Nothing
                   c <- invokeEvent p $ unfreezeCont c
                   case c of
                     Nothing -> return ()
                     Just c  -> invokeEvent p $ enqueueEvent (pointTime p) $ resumeCont c ()

-- | Require to return an identifier of the process associated with the transact.
requireTransactProcessId :: MonadDES m => Transact m a -> Event m (ProcessId m)
{-# INLINABLE requireTransactProcessId #-}
requireTransactProcessId t =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (transactProcessIdRef t)
     case a of
       Nothing ->
         throwComp $
         SimulationRetry
         "The transact must be associated with any process: requireTransactProcessId"
       Just pid ->
         return pid

-- | Like the GoTo statement, it associates the transact with another process.
transferTransact :: MonadDES m => Transact m a -> Process m () -> Event m ()
{-# INLINABLE transferTransact #-}
transferTransact t transfer =
  Event $ \p ->
  do pid <- invokeEvent p $ requireTransactProcessId t
     invokeEvent p $ cancelProcessWithId pid
     invokeEvent p $ writeRef (transactProcessIdRef t) Nothing
     invokeEvent p $ writeRef (transactProcessContRef t) Nothing
     invokeEvent p $
       runProcess $
       do takeTransact t
          transferProcess transfer

-- | Register the queue entry in the transact.
registerTransactQueueEntry :: MonadDES m => Transact m a -> QueueEntry m -> Event m ()
{-# INLINABLE registerTransactQueueEntry #-}
registerTransactQueueEntry t e =
  Event $ \p ->
  do let q = entryQueue e
     m <- invokeEvent p $ readRef (transactQueueEntryRef t)
     case HM.lookup q m of
       Just e0 ->
         throwComp $
         SimulationRetry
         "There is already another queue entry for the specified queue: registerTransactQueueEntry"
       Nothing ->
         invokeEvent p $ writeRef (transactQueueEntryRef t) (HM.insert q e m)

-- | Unregister the queue entry from the transact.
unregisterTransactQueueEntry :: MonadDES m => Transact m a -> Queue m -> Event m (QueueEntry m)
{-# INLINABLE unregisterTransactQueueEntry #-}
unregisterTransactQueueEntry t q =
  Event $ \p ->
  do m <- invokeEvent p $ readRef (transactQueueEntryRef t)
     case HM.lookup q m of
       Nothing ->
         throwComp $
         SimulationRetry
         "There is no queue entry for the specified queue: unregisterTransactQueueEntry"
       Just e  ->
         do invokeEvent p $ writeRef (transactQueueEntryRef t) (HM.delete q m)
            return e
