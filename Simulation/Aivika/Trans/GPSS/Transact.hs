
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
        transactArrivalTime,
        transactPriority,
        takeTransact,
        releaseTransact,
        transactPreemptionBegin,
        transactPreemptionEnd) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Internal.Specs
import Simulation.Aivika.Trans.Internal.Event
import Simulation.Aivika.Trans.Internal.Cont
import Simulation.Aivika.Trans.Internal.Process

-- | Represents a GPSS transact.
data Transact m a =
  Transact { transactValue :: a,
             -- ^ The data of the transact.
             transactArrivalTime :: Double,
             -- ^ The time at which the transact was generated.
             transactPriority :: Int,
             -- ^ The transact priority.
             transactPreemptionCountRef :: Ref m Int,
             -- ^ How many times the transact is preempted.
             transactProcessIdRef :: Ref m (Maybe (ProcessId m)),
             -- ^ An identifier of the process that handles the transact at present
             transactProcessContRef :: Ref m (Maybe (FrozenCont m ()))
             -- ^ A continuation of the process that tried to handle the transact.
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
       Just pid0 -> error "The transact is acquired by another process: takeTransact"
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
         error "The transact is not acquired by any process: releaseTransact"
       Just pid0 | pid0 /= pid ->
         error "The transact is acquired by another process: releaseTransact"
       Just pid0 ->
         do invokeEvent p $ writeRef (transactProcessIdRef t) Nothing
            n <- invokeEvent p $ readRef (transactPreemptionCountRef t)
            unless (n == 0) $
              error "The transact cannot be preempted in this state: releaseTransact"
            c0 <- invokeEvent p $ readRef (transactProcessContRef t)
            case c0 of
              Nothing -> invokeEvent p $ resumeCont c ()
              Just c0 -> error "The transact process cannot be frozen in this state: releaseTransact"

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
       error "The transact preemption count cannot be negative: transactPreemptionEnd"
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
