
-- |
-- Module     : Simulation.Aivika.GPSS.Transact
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines a GPSS transact.
--
module Simulation.Aivika.GPSS.Transact
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

import Data.IORef
import qualified Data.HashMap.Lazy as HM

import Simulation.Aivika
import Simulation.Aivika.Internal.Specs
import Simulation.Aivika.Internal.Simulation
import Simulation.Aivika.Internal.Event
import Simulation.Aivika.Internal.Cont
import Simulation.Aivika.Internal.Process

import {-# SOURCE #-} Simulation.Aivika.GPSS.Queue

-- | Represents a GPSS transact.
data Transact a =
  Transact { transactValue :: a,
             -- ^ The data of the transact.
             transactArrivalDelay :: Maybe Double,
             -- ^ The delay between the transacts generated.
             transactArrivalTime :: Double,
             -- ^ The time at which the transact was generated.
             transactPriority :: Int,
             -- ^ The transact priority.
             transactPreemptionCountRef :: IORef Int,
             -- ^ How many times the transact is preempted.
             transactProcessIdRef :: IORef (Maybe ProcessId),
             -- ^ An identifier of the process that handles the transact at present
             transactProcessContRef :: IORef (Maybe (FrozenCont ())),
             -- ^ A continuation of the process that tried to handle the transact.
             transactQueueEntryRef :: IORef (HM.HashMap Queue QueueEntry)
             -- ^ The queue entries registered by the the transact.
           }

instance Eq (Transact a) where
  x == y = (transactProcessIdRef x) == (transactProcessIdRef y)

-- | Create a new transact.
newTransact :: Arrival a
               -- ^ the arrival data
               -> Int
               -- ^ the transact priority
               -> Simulation (Transact a)
newTransact a priority =
  Simulation $ \r ->
  do r0 <- newIORef 0
     r1 <- newIORef Nothing
     r2 <- newIORef Nothing
     r3 <- newIORef HM.empty
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
takeTransact :: Transact a -> Process ()
takeTransact t =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do pid0 <- readIORef (transactProcessIdRef t)
     case pid0 of
       Just pid0 ->
         throwIO $
         SimulationRetry
         "The transact is acquired by another process: takeTransact"
       Nothing   ->
         do writeIORef (transactProcessIdRef t) (Just pid)
            n <- readIORef (transactPreemptionCountRef t)
            if n == 0
              then invokeEvent p $ resumeCont c ()
              else do c <- invokeEvent p $
                           freezeContReentering c () $
                           invokeCont c $
                           invokeProcess pid $
                           takeTransact t
                      writeIORef (transactProcessContRef t) (Just c)
                      forM_ [1 .. n] $ \_ ->
                        invokeEvent p $
                        processPreemptionBegin pid

-- | Release the transact.
releaseTransact :: Transact a -> Process ()
releaseTransact t =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do pid0 <- readIORef (transactProcessIdRef t)
     case pid0 of
       Nothing ->
         throwIO $
         SimulationRetry
         "The transact is not acquired by any process: releaseTransact"
       Just pid0 | pid0 /= pid ->
         throwIO $
         SimulationRetry
         "The transact is acquired by another process: releaseTransact"
       Just pid0 ->
         do writeIORef (transactProcessIdRef t) Nothing
            writeIORef (transactProcessContRef t) Nothing
            invokeEvent p $ resumeCont c ()

-- | Preempt the computation that handles the transact.
transactPreemptionBegin :: Transact a -> Event ()
transactPreemptionBegin t =
  Event $ \p ->
  do n <- readIORef (transactPreemptionCountRef t)
     let n' = n + 1
     n' `seq` writeIORef (transactPreemptionCountRef t) n'
     pid <- readIORef (transactProcessIdRef t)
     case pid of
       Nothing  -> return ()
       Just pid -> invokeEvent p $ processPreemptionBegin pid

-- | Proceed with the computation after the transact was preempted earlier.
transactPreemptionEnd :: Transact a -> Event ()
transactPreemptionEnd t =
  Event $ \p ->
  do n <- readIORef (transactPreemptionCountRef t)
     let n' = n - 1
     unless (n' >= 0) $
       throwIO $
       SimulationRetry
       "The transact preemption count cannot be negative: transactPreemptionEnd"
     n' `seq` writeIORef (transactPreemptionCountRef t) n'
     pid <- readIORef (transactProcessIdRef t)
     case pid of
       Nothing  -> return ()
       Just pid ->
         do invokeEvent p $ processPreemptionEnd pid
            c <- readIORef (transactProcessContRef t)
            case c of
              Nothing -> return ()
              Just c  ->
                do writeIORef (transactProcessContRef t) Nothing
                   c <- invokeEvent p $ unfreezeCont c
                   case c of
                     Nothing -> return ()
                     Just c  -> invokeEvent p $ enqueueEvent (pointTime p) $ resumeCont c ()

-- | Require to return an identifier of the process associated with the transact.
requireTransactProcessId :: Transact a -> Event ProcessId
requireTransactProcessId t =
  Event $ \p ->
  do a <- readIORef (transactProcessIdRef t)
     case a of
       Nothing ->
         throwIO $
         SimulationRetry
         "The transact must be associated with any process: requireTransactProcessId"
       Just pid ->
         return pid

-- | Like the GoTo statement, it associates the transact with another process.
transferTransact :: Transact a -> Process () -> Event ()
transferTransact t transfer =
  Event $ \p ->
  do pid <- invokeEvent p $ requireTransactProcessId t
     invokeEvent p $ cancelProcessWithId pid
     writeIORef (transactProcessIdRef t) Nothing
     writeIORef (transactProcessContRef t) Nothing
     invokeEvent p $
       runProcess $
       do takeTransact t
          transferProcess transfer

-- | Register the queue entry in the transact.
registerTransactQueueEntry :: Transact a -> QueueEntry -> Event ()
registerTransactQueueEntry t e =
  Event $ \p ->
  do let q = entryQueue e
     m <- readIORef (transactQueueEntryRef t)
     case HM.lookup q m of
       Just e0 ->
         throwIO $
         SimulationRetry
         "There is already another queue entry for the specified queue: registerTransactQueueEntry"
       Nothing ->
         writeIORef (transactQueueEntryRef t) (HM.insert q e m)

-- | Unregister the queue entry from the transact.
unregisterTransactQueueEntry :: Transact a -> Queue -> Event QueueEntry
unregisterTransactQueueEntry t q =
  Event $ \p ->
  do m <- readIORef (transactQueueEntryRef t)
     case HM.lookup q m of
       Nothing ->
         throwIO $
         SimulationRetry
         "There is no queue entry for the specified queue: unregisterTransactQueueEntry"
       Just e  ->
         do writeIORef (transactQueueEntryRef t) (HM.delete q m)
            return e
