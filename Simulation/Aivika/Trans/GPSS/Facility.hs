
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Facility
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines the GPSS Facility entity.
--
module Simulation.Aivika.Trans.GPSS.Facility
       (-- * Facility Type
        Facility,
        FacilityPreemptMode(..),
        FacilityPreemptTransfer,
        -- * Creating Facility
        newFacility,
        -- * Facility Properties
        facilityCount,
        facilityCountStats,
        facilityCaptureCount,
        facilityUtilisationCount,
        facilityUtilisationCountStats,
        facilityQueueCount,
        facilityQueueCountStats,
        facilityTotalWaitTime,
        facilityWaitTime,
        facilityTotalHoldingTime,
        facilityHoldingTime,
        facilityInterrupted,
        -- * Seizing-Releasing and Preempting-Returning Facility
        seizeFacility,
        releaseFacility,
        preemptFacility,
        returnFacility,
        -- * Statistics Reset
        resetFacility,
        -- * Signals
        facilityCountChanged,
        facilityCountChanged_,
        facilityCaptureCountChanged,
        facilityCaptureCountChanged_,
        facilityUtilisationCountChanged,
        facilityUtilisationCountChanged_,
        facilityQueueCountChanged,
        facilityQueueCountChanged_,
        facilityWaitTimeChanged,
        facilityWaitTimeChanged_,
        facilityHoldingTimeChanged,
        facilityHoldingTimeChanged_,
        facilityChanged_) where

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

-- | Represents a GPSS Facility entity.
data Facility m a = 
  Facility { facilityCountRef :: Ref m Int,
             facilityCountStatsRef :: Ref m (TimingStats Int),
             facilityCountSource :: SignalSource m Int,
             facilityCaptureCountRef :: Ref m Int,
             facilityCaptureCountSource :: SignalSource m Int,
             facilityUtilisationCountRef :: Ref m Int,
             facilityUtilisationCountStatsRef :: Ref m (TimingStats Int),
             facilityUtilisationCountSource :: SignalSource m Int,
             facilityQueueCountRef :: Ref m Int,
             facilityQueueCountStatsRef :: Ref m (TimingStats Int),
             facilityQueueCountSource :: SignalSource m Int,
             facilityTotalWaitTimeRef :: Ref m Double,
             facilityWaitTimeRef :: Ref m (SamplingStats Double),
             facilityWaitTimeSource :: SignalSource m (),
             facilityTotalHoldingTimeRef :: Ref m Double,
             facilityHoldingTimeRef :: Ref m (SamplingStats Double),
             facilityHoldingTimeSource :: SignalSource m (),
             facilityOwnerRef :: Ref m (Maybe (FacilityOwnerItem m a)),
             facilityDelayChain :: StrategyQueue m (TransactQueueStrategy FCFS) (FacilityDelayedItem m a),
             facilityInterruptChain :: StrategyQueue m (TransactQueueStrategy LCFS) (FacilityInterruptedItem m a),
             facilityPendingChain :: StrategyQueue m (TransactQueueStrategy LCFS) (FacilityPendingItem m a) }

-- | Identifies a transact item that owns the facility.
data FacilityOwnerItem m a =
  FacilityOwnerItem { ownerItemTransact :: Transact m a,
                      ownerItemTime :: Double,
                      ownerItemPreempting :: Bool,
                      ownerItemAccHoldingTime :: Double }

-- | Idenitifies a transact item that was delayed.
data FacilityDelayedItem m a =
  FacilityDelayedItem { delayedItemTransact :: Transact m a,
                        delayedItemTime :: Double,
                        delayedItemPreempting :: Bool,
                        delayedItemCont :: FrozenCont m () }

-- | Idenitifies a transact item that was interrupted.
data FacilityInterruptedItem m a =
  FacilityInterruptedItem { interruptedItemTransact :: Transact m a,
                            interruptedItemTime :: Double,
                            interruptedItemPreempting :: Bool,
                            interruptedItemRemainingTime :: Maybe Double,
                            interruptedItemTransfer :: Maybe (FacilityPreemptTransfer m a),
                            interruptedItemAccHoldingTime :: Double }

-- | Idenitifies a transact item which is pending.
data FacilityPendingItem m a =
  FacilityPendingItem { pendingItemTransact :: Transact m a,
                        pendingItemTime :: Double,
                        pendingItemPreempting :: Bool,
                        pendingItemCont :: FrozenCont m () }

instance MonadDES m => Eq (Facility m a) where
  x == y = facilityCountRef x == facilityCountRef y  -- unique references

-- | The facility preemption mode.
data FacilityPreemptMode m a =
  FacilityPreemptMode { facilityPriorityMode :: Bool,
                        -- ^ the Priority mode; otherwise, the Interrupt mode
                        facilityTransfer :: Maybe (FacilityPreemptTransfer m a),
                        -- ^ where to transfer the preempted transact,
                        -- passing in the remaining time from the process holding
                        -- computation such as the ADVANCE block
                        facilityRemoveMode :: Bool
                        -- ^ the Remove mode
                      }

-- | Proceed with the computation by the specified preempted transact
-- and remaining time from the process holding computation such as the ADVANCE block.
type FacilityPreemptTransfer m a = Transact m a -> Maybe Double -> Process m ()

-- | The default facility preemption mode.
defaultFacilityPreemptMode :: FacilityPreemptMode m a
defaultFacilityPreemptMode =
  FacilityPreemptMode { facilityPriorityMode = False,
                        facilityTransfer = Nothing,
                        facilityRemoveMode = False
                      }

-- | Create a new facility.
newFacility :: MonadDES m => Event m (Facility m a)
{-# INLINABLE newFacility #-}
newFacility =
  Event $ \p ->
  do let r = pointRun p
         t = pointTime p
     countRef <- invokeSimulation r $ newRef 1
     countStatsRef <- invokeSimulation r $ newRef $ returnTimingStats t 1
     countSource <- invokeSimulation r newSignalSource
     captureCountRef <- invokeSimulation r $ newRef 0
     captureCountSource <- invokeSimulation r newSignalSource
     utilCountRef <- invokeSimulation r $ newRef 0
     utilCountStatsRef <- invokeSimulation r $ newRef $ returnTimingStats t 0
     utilCountSource <- invokeSimulation r newSignalSource
     queueCountRef <- invokeSimulation r $ newRef 0
     queueCountStatsRef <- invokeSimulation r $ newRef $ returnTimingStats t 0
     queueCountSource <- invokeSimulation r newSignalSource
     totalWaitTimeRef <- invokeSimulation r $ newRef 0
     waitTimeRef <- invokeSimulation r $ newRef emptySamplingStats
     waitTimeSource <- invokeSimulation r newSignalSource
     totalHoldingTimeRef <- invokeSimulation r $ newRef 0
     holdingTimeRef <- invokeSimulation r $ newRef emptySamplingStats
     holdingTimeSource <- invokeSimulation r newSignalSource
     ownerRef <- invokeSimulation r $ newRef Nothing
     delayChain <- invokeSimulation r $ newStrategyQueue (TransactQueueStrategy FCFS)
     interruptChain <- invokeSimulation r $ newStrategyQueue (TransactQueueStrategy LCFS)
     pendingChain <- invokeSimulation r $ newStrategyQueue (TransactQueueStrategy LCFS)
     return Facility { facilityCountRef = countRef,
                       facilityCountStatsRef = countStatsRef,
                       facilityCountSource = countSource,
                       facilityCaptureCountRef = captureCountRef,
                       facilityCaptureCountSource = captureCountSource,
                       facilityUtilisationCountRef = utilCountRef,
                       facilityUtilisationCountStatsRef = utilCountStatsRef,
                       facilityUtilisationCountSource = utilCountSource,
                       facilityQueueCountRef = queueCountRef,
                       facilityQueueCountStatsRef = queueCountStatsRef,
                       facilityQueueCountSource = queueCountSource,
                       facilityTotalWaitTimeRef = totalWaitTimeRef,
                       facilityWaitTimeRef = waitTimeRef,
                       facilityWaitTimeSource = waitTimeSource,
                       facilityTotalHoldingTimeRef = totalHoldingTimeRef,
                       facilityHoldingTimeRef = holdingTimeRef,
                       facilityHoldingTimeSource = holdingTimeSource,
                       facilityOwnerRef = ownerRef,
                       facilityDelayChain = delayChain,
                       facilityInterruptChain = interruptChain,
                       facilityPendingChain = pendingChain }

-- | Return the current available count of the facility.
facilityCount :: MonadDES m => Facility m a -> Event m Int
{-# INLINABLE facilityCount #-}
facilityCount r =
  Event $ \p -> invokeEvent p $ readRef (facilityCountRef r)

-- | Return the statistics for the available count of the facility.
facilityCountStats :: MonadDES m => Facility m a -> Event m (TimingStats Int)
{-# INLINABLE facilityCountStats #-}
facilityCountStats r =
  Event $ \p -> invokeEvent p $ readRef (facilityCountStatsRef r)

-- | Signal triggered when the 'facilityCount' property changes.
facilityCountChanged :: MonadDES m => Facility m a -> Signal m Int
{-# INLINABLE facilityCountChanged #-}
facilityCountChanged r =
  publishSignal $ facilityCountSource r

-- | Signal triggered when the 'facilityCount' property changes.
facilityCountChanged_ :: MonadDES m => Facility m a -> Signal m ()
{-# INLINABLE facilityCountChanged_ #-}
facilityCountChanged_ r =
  mapSignal (const ()) $ facilityCountChanged r

-- | Return the current capture count of the facility.
facilityCaptureCount :: MonadDES m => Facility m a -> Event m Int
{-# INLINABLE facilityCaptureCount #-}
facilityCaptureCount r =
  Event $ \p -> invokeEvent p $ readRef (facilityCaptureCountRef r)

-- | Signal triggered when the 'facilityCaptureCount' property changes.
facilityCaptureCountChanged :: MonadDES m => Facility m a -> Signal m Int
{-# INLINABLE facilityCaptureCountChanged #-}
facilityCaptureCountChanged r =
  publishSignal $ facilityCaptureCountSource r

-- | Signal triggered when the 'facilityCaptureCount' property changes.
facilityCaptureCountChanged_ :: MonadDES m => Facility m a -> Signal m ()
{-# INLINABLE facilityCaptureCountChanged_ #-}
facilityCaptureCountChanged_ r =
  mapSignal (const ()) $ facilityCaptureCountChanged r

-- | Return the current utilisation count of the facility.
facilityUtilisationCount :: MonadDES m => Facility m a -> Event m Int
{-# INLINABLE facilityUtilisationCount #-}
facilityUtilisationCount r =
  Event $ \p -> invokeEvent p $ readRef (facilityUtilisationCountRef r)

-- | Return the statistics for the utilisation count of the facility.
facilityUtilisationCountStats :: MonadDES m => Facility m a -> Event m (TimingStats Int)
{-# INLINABLE facilityUtilisationCountStats #-}
facilityUtilisationCountStats r =
  Event $ \p -> invokeEvent p $ readRef (facilityUtilisationCountStatsRef r)

-- | Signal triggered when the 'facilityUtilisationCount' property changes.
facilityUtilisationCountChanged :: MonadDES m => Facility m a -> Signal m Int
{-# INLINABLE facilityUtilisationCountChanged #-}
facilityUtilisationCountChanged r =
  publishSignal $ facilityUtilisationCountSource r

-- | Signal triggered when the 'facilityUtilisationCount' property changes.
facilityUtilisationCountChanged_ :: MonadDES m => Facility m a -> Signal m ()
{-# INLINABLE facilityUtilisationCountChanged_ #-}
facilityUtilisationCountChanged_ r =
  mapSignal (const ()) $ facilityUtilisationCountChanged r

-- | Return the current queue length of the facility.
facilityQueueCount :: MonadDES m => Facility m a -> Event m Int
{-# INLINABLE facilityQueueCount #-}
facilityQueueCount r =
  Event $ \p -> invokeEvent p $ readRef (facilityQueueCountRef r)

-- | Return the statistics for the queue length of the facility.
facilityQueueCountStats :: MonadDES m => Facility m a -> Event m (TimingStats Int)
{-# INLINABLE facilityQueueCountStats #-}
facilityQueueCountStats r =
  Event $ \p -> invokeEvent p $ readRef (facilityQueueCountStatsRef r)

-- | Signal triggered when the 'facilityQueueCount' property changes.
facilityQueueCountChanged :: MonadDES m => Facility m a -> Signal m Int
{-# INLINABLE facilityQueueCountChanged #-}
facilityQueueCountChanged r =
  publishSignal $ facilityQueueCountSource r

-- | Signal triggered when the 'facilityQueueCount' property changes.
facilityQueueCountChanged_ :: MonadDES m => Facility m a -> Signal m ()
{-# INLINABLE facilityQueueCountChanged_ #-}
facilityQueueCountChanged_ r =
  mapSignal (const ()) $ facilityQueueCountChanged r

-- | Return the total wait time of the facility.
facilityTotalWaitTime :: MonadDES m => Facility m a -> Event m Double
{-# INLINABLE facilityTotalWaitTime #-}
facilityTotalWaitTime r =
  Event $ \p -> invokeEvent p $ readRef (facilityTotalWaitTimeRef r)

-- | Return the statistics for the wait time of the facility.
facilityWaitTime :: MonadDES m => Facility m a -> Event m (SamplingStats Double)
{-# INLINABLE facilityWaitTime #-}
facilityWaitTime r =
  Event $ \p -> invokeEvent p $ readRef (facilityWaitTimeRef r)

-- | Signal triggered when the 'facilityTotalWaitTime' and 'facilityWaitTime' properties change.
facilityWaitTimeChanged :: MonadDES m => Facility m a -> Signal m (SamplingStats Double)
{-# INLINABLE facilityWaitTimeChanged #-}
facilityWaitTimeChanged r =
  mapSignalM (\() -> facilityWaitTime r) $ facilityWaitTimeChanged_ r

-- | Signal triggered when the 'facilityTotalWaitTime' and 'facilityWaitTime' properties change.
facilityWaitTimeChanged_ :: MonadDES m => Facility m a -> Signal m ()
{-# INLINABLE facilityWaitTimeChanged_ #-}
facilityWaitTimeChanged_ r =
  publishSignal $ facilityWaitTimeSource r

-- | Return the total holding time of the facility.
facilityTotalHoldingTime :: MonadDES m => Facility m a -> Event m Double
{-# INLINABLE facilityTotalHoldingTime #-}
facilityTotalHoldingTime r =
  Event $ \p -> invokeEvent p $ readRef (facilityTotalHoldingTimeRef r)

-- | Return the statistics for the holding time of the facility.
facilityHoldingTime :: MonadDES m => Facility m a -> Event m (SamplingStats Double)
{-# INLINABLE facilityHoldingTime #-}
facilityHoldingTime r =
  Event $ \p -> invokeEvent p $ readRef (facilityHoldingTimeRef r)

-- | Signal triggered when the 'facilityTotalHoldingTime' and 'facilityHoldingTime' properties change.
facilityHoldingTimeChanged :: MonadDES m => Facility m a -> Signal m (SamplingStats Double)
{-# INLINABLE facilityHoldingTimeChanged #-}
facilityHoldingTimeChanged r =
  mapSignalM (\() -> facilityHoldingTime r) $ facilityHoldingTimeChanged_ r

-- | Signal triggered when the 'facilityTotalHoldingTime' and 'facilityHoldingTime' properties change.
facilityHoldingTimeChanged_ :: MonadDES m => Facility m a -> Signal m ()
{-# INLINABLE facilityHoldingTimeChanged_ #-}
facilityHoldingTimeChanged_ r =
  publishSignal $ facilityHoldingTimeSource r

-- | Whether the facility is currently interrupted.
facilityInterrupted :: MonadDES m => Facility m a -> Event m Bool
{-# INLINABLE facilityInterrupted #-}
facilityInterrupted r =
  Event $ \p ->
  do x <- invokeEvent p $ readRef (facilityOwnerRef r)
     case x of
       Nothing -> return False
       Just a  -> return (ownerItemPreempting a)

-- | Seize the facility.
seizeFacility :: MonadDES m
                 => Facility m a
                 -- ^ the requested facility
                 -> Transact m a
                 -- ^ the transact that tries to seize the facility
                 -> Process m ()
{-# INLINABLE seizeFacility #-}
seizeFacility r transact =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do let t = pointTime p
     f <- do f1 <- invokeEvent p $ strategyQueueNull (facilityDelayChain r)
             if f1
               then do f2 <- invokeEvent p $ strategyQueueNull (facilityInterruptChain r)
                       if f2
                         then invokeEvent p $ strategyQueueNull (facilityPendingChain r)
                         else return False
               else return False
     if f
       then invokeEvent p $
            invokeCont c $
            invokeProcess pid $
            seizeFacility' r transact
       else do c <- invokeEvent p $
                    freezeContReentering c () $
                    invokeCont c $
                    invokeProcess pid $
                    seizeFacility r transact
               invokeEvent p $
                 strategyEnqueueWithPriority
                 (facilityDelayChain r)
                 (transactPriority transact)
                 (FacilityDelayedItem transact t False c)
               invokeEvent p $ updateFacilityQueueCount r 1

-- | Seize the facility.
seizeFacility' :: MonadDES m
                  => Facility m a
                  -- ^ the requested facility
                  -> Transact m a
                  -- ^ the transact that tries to seize the facility
                  -> Process m ()
{-# INLINABLE seizeFacility' #-}
seizeFacility' r transact =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do let t = pointTime p
     a <- invokeEvent p $ readRef (facilityOwnerRef r)
     case a of
       Nothing ->
         do invokeEvent p $ writeRef (facilityOwnerRef r) $ Just (FacilityOwnerItem transact t False 0)
            invokeEvent p $ updateFacilityWaitTime r 0
            invokeEvent p $ updateFacilityCount r (-1)
            invokeEvent p $ updateFacilityCaptureCount r 1
            invokeEvent p $ updateFacilityUtilisationCount r 1
            invokeEvent p $ resumeCont c ()
       Just owner ->
         do c <- invokeEvent p $
                 freezeContReentering c () $
                 invokeCont c $
                 invokeProcess pid $
                 seizeFacility r transact
            invokeEvent p $
              strategyEnqueueWithPriority
              (facilityDelayChain r)
              (transactPriority transact)
              (FacilityDelayedItem transact t False c)
            invokeEvent p $ updateFacilityQueueCount r 1

-- | Preempt the facility.
preemptFacility :: MonadDES m
                   => Facility m a
                   -- ^ the requested facility
                   -> Transact m a
                   -- ^ the transact that tries to preempt the facility
                   -> FacilityPreemptMode m a
                   -- ^ the Preempt mode
                   -> Process m ()
{-# INLINABLE preemptFacility #-}
preemptFacility r transact mode =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do let t = pointTime p
     a <- invokeEvent p $ readRef (facilityOwnerRef r)
     case a of
       Nothing ->
         do invokeEvent p $ writeRef (facilityOwnerRef r) $ Just (FacilityOwnerItem transact t True 0)
            invokeEvent p $ updateFacilityWaitTime r 0
            invokeEvent p $ updateFacilityCount r (-1)
            invokeEvent p $ updateFacilityCaptureCount r 1
            invokeEvent p $ updateFacilityUtilisationCount r 1
            invokeEvent p $ resumeCont c ()
       Just owner@(FacilityOwnerItem transact0 t0 preempting0 acc0)
         | (not $ facilityPriorityMode mode) && preempting0 ->
         do c <- invokeEvent p $
                 freezeContReentering c () $
                 invokeCont c $
                 invokeProcess pid $
                 preemptFacility r transact mode
            invokeEvent p $
              strategyEnqueueWithPriority
              (facilityPendingChain r)
              (transactPriority transact)
              (FacilityPendingItem transact t True c)
            invokeEvent p $ updateFacilityQueueCount r 1
       Just owner@(FacilityOwnerItem transact0 t0 preempting0 acc0)
         | facilityPriorityMode mode && (transactPriority transact <= transactPriority transact0) ->
         do c <- invokeEvent p $
                 freezeContReentering c () $
                 invokeCont c $
                 invokeProcess pid $
                 preemptFacility r transact mode
            invokeEvent p $
              strategyEnqueueWithPriority
              (facilityDelayChain r)
              (transactPriority transact)
              (FacilityDelayedItem transact t True c)
            invokeEvent p $ updateFacilityQueueCount r 1
       Just owner@(FacilityOwnerItem transact0 t0 preempting0 acc0)
         | (not $ facilityRemoveMode mode) ->
         do invokeEvent p $ writeRef (facilityOwnerRef r) $ Just (FacilityOwnerItem transact t True 0)
            pid0 <- invokeEvent p $ requireTransactProcessId transact0
            t2   <- invokeEvent p $ processInterruptionTime pid0
            let dt0 = fmap (\x -> x - t) t2
            invokeEvent p $
              strategyEnqueueWithPriority
              (facilityInterruptChain r)
              (transactPriority transact0)
              (FacilityInterruptedItem transact0 t preempting0 dt0 (facilityTransfer mode) (acc0 + (t - t0)))
            invokeEvent p $ updateFacilityQueueCount r 1
            invokeEvent p $ updateFacilityWaitTime r 0
            invokeEvent p $ updateFacilityCaptureCount r 1
            invokeEvent p $ transactPreemptionBegin transact0
            invokeEvent p $ resumeCont c ()
       Just owner@(FacilityOwnerItem transact0 t0 preempting0 acc0)
         | facilityRemoveMode mode ->
         do invokeEvent p $ writeRef (facilityOwnerRef r) $ Just (FacilityOwnerItem transact t True 0)
            pid0 <- invokeEvent p $ requireTransactProcessId transact0
            t2   <- invokeEvent p $ processInterruptionTime pid0
            let dt0 = fmap (\x -> x - t) t2
            invokeEvent p $ updateFacilityWaitTime r 0
            invokeEvent p $ updateFacilityCaptureCount r 1
            invokeEvent p $ updateFacilityHoldingTime r (acc0 + (t - t0))
            case facilityTransfer mode of
              Nothing ->
                throwComp $
                SimulationRetry
                "The transfer destination is not specified for the removed preempted transact: preemptFacility"
              Just transfer ->
                invokeEvent p $ transferTransact transact0 (transfer transact0 dt0)
            invokeEvent p $ resumeCont c ()

-- | Return the facility by the active transact.
returnFacility :: MonadDES m
                  => Facility m a
                  -- ^ the facility to return
                  -> Transact m a
                  -- ^ the active transact that tries to return the facility
                  -> Process m ()
{-# INLINABLE returnFacility #-}
returnFacility r transact = releaseFacility' r transact True 

-- | Release the facility by the active transact.
releaseFacility :: MonadDES m
                   => Facility m a
                   -- ^ the facility to release
                   -> Transact m a
                   -- ^ the active transact that tries to release the facility
                   -> Process m ()
{-# INLINABLE releaseFacility #-}
releaseFacility r transact = releaseFacility' r transact False 

-- | Release the facility by the active transact.
releaseFacility' :: MonadDES m
                    => Facility m a
                    -- ^ the facility to release
                    -> Transact m a
                    -- ^ the active transact that tries to release the facility
                    -> Bool
                    -- ^ whether the transact is preempting
                    -> Process m ()
{-# INLINABLE releaseFacility' #-}
releaseFacility' r transact preempting = 
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do let t = pointTime p
     a <- invokeEvent p $ readRef (facilityOwnerRef r)
     case a of
       Nothing ->
         throwComp $
         SimulationRetry
         "There is no owner of the facility: releaseFacility'"
       Just owner@(FacilityOwnerItem transact0 t0 preempting0 acc0) | transact0 == transact && preempting0 /= preempting ->
         throwComp $
         SimulationRetry
         "The mismatch use of releaseFacility and returnFacility: releaseFacility'"
       Just owner@(FacilityOwnerItem transact0 t0 preempting0 acc0) | transact0 == transact ->
         do invokeEvent p $ writeRef (facilityOwnerRef r) Nothing
            invokeEvent p $ updateFacilityUtilisationCount r (-1)
            invokeEvent p $ updateFacilityHoldingTime r (acc0 + (t - t0))
            invokeEvent p $ updateFacilityCount r 1
            invokeEvent p $ enqueueEvent t $ tryCaptureFacility r
            invokeEvent p $ resumeCont c ()
       Just owner ->
         throwComp $
         SimulationRetry
         "The facility has another owner: releaseFacility'"

-- | Try to capture the facility.
tryCaptureFacility :: MonadDES m => Facility m a -> Event m ()
{-# INLINABLE tryCaptureFacility #-}
tryCaptureFacility r =
  Event $ \p ->
  do let t = pointTime p
     a <- invokeEvent p $ readRef (facilityOwnerRef r)
     case a of
       Nothing ->
         invokeEvent p $ captureFacility r
       Just owner -> return ()

-- | Find another owner of the facility.
captureFacility :: MonadDES m => Facility m a -> Event m ()
{-# INLINABLE captureFacility #-}
captureFacility r =
  Event $ \p ->
  do let t = pointTime p
     f <- invokeEvent p $ strategyQueueNull (facilityPendingChain r)
     if not f
       then do FacilityPendingItem transact t0 preempting c0 <- invokeEvent p $ strategyDequeue (facilityPendingChain r)
               invokeEvent p $ updateFacilityQueueCount r (-1)
               c <- invokeEvent p $ unfreezeCont c0
               case c of
                 Nothing ->
                   invokeEvent p $ captureFacility r
                 Just c ->
                   do invokeEvent p $ writeRef (facilityOwnerRef r) $ Just (FacilityOwnerItem transact t preempting 0)
                      invokeEvent p $ updateFacilityWaitTime r (t - t0)
                      invokeEvent p $ updateFacilityUtilisationCount r 1
                      invokeEvent p $ updateFacilityCaptureCount r 1
                      invokeEvent p $ updateFacilityCount r (-1)
                      invokeEvent p $ enqueueEvent t $ reenterCont c ()
       else do f <- invokeEvent p $ strategyQueueNull (facilityInterruptChain r)
               if not f
                  then do FacilityInterruptedItem transact t0 preempting dt0 transfer0 acc0 <- invokeEvent p $ strategyDequeue (facilityInterruptChain r)
                          pid <- invokeEvent p $ requireTransactProcessId transact
                          invokeEvent p $ updateFacilityQueueCount r (-1)
                          f <- invokeEvent p $ processCancelled pid
                          case f of
                            True ->
                              invokeEvent p $ captureFacility r
                            False ->
                              do invokeEvent p $ writeRef (facilityOwnerRef r) $ Just (FacilityOwnerItem transact t preempting acc0)
                                 invokeEvent p $ updateFacilityWaitTime r (t - t0)
                                 invokeEvent p $ updateFacilityUtilisationCount r 1
                                 invokeEvent p $ updateFacilityCount r (-1)
                                 case transfer0 of
                                   Nothing -> return ()
                                   Just transfer ->
                                     invokeEvent p $ transferTransact transact (transfer transact dt0)
                                 invokeEvent p $ transactPreemptionEnd transact
                 else do f <- invokeEvent p $ strategyQueueNull (facilityDelayChain r)
                         if not f
                           then do FacilityDelayedItem transact t0 preempting c0 <- invokeEvent p $ strategyDequeue (facilityDelayChain r)
                                   invokeEvent p $ updateFacilityQueueCount r (-1)
                                   c <- invokeEvent p $ unfreezeCont c0
                                   case c of
                                     Nothing ->
                                       invokeEvent p $ captureFacility r
                                     Just c ->
                                       do invokeEvent p $ writeRef (facilityOwnerRef r) $ Just (FacilityOwnerItem transact t preempting 0)
                                          invokeEvent p $ updateFacilityWaitTime r (t - t0)
                                          invokeEvent p $ updateFacilityUtilisationCount r 1
                                          invokeEvent p $ updateFacilityCaptureCount r 1
                                          invokeEvent p $ updateFacilityCount r (-1)
                                          invokeEvent p $ enqueueEvent t $ reenterCont c ()
                           else return ()

-- | Signal triggered when one of the facility counters changes.
facilityChanged_ :: MonadDES m => Facility m a -> Signal m ()
{-# INLINABLE facilityChanged_ #-}
facilityChanged_ r =
  facilityCountChanged_ r <>
  facilityCaptureCountChanged_ r <>
  facilityUtilisationCountChanged_ r <>
  facilityQueueCountChanged_ r

-- | Update the facility count and its statistics.
updateFacilityCount :: MonadDES m => Facility m a -> Int -> Event m ()
{-# INLINABLE updateFacilityCount #-}
updateFacilityCount r delta =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (facilityCountRef r)
     let a' = a + delta
     invokeEvent p $
       writeRef (facilityCountRef r) a'
     invokeEvent p $
       modifyRef (facilityCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (facilityCountSource r) a'

-- | Update the facility capture count.
updateFacilityCaptureCount :: MonadDES m => Facility m a -> Int -> Event m ()
{-# INLINABLE updateFacilityCaptureCount #-}
updateFacilityCaptureCount r delta =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (facilityCaptureCountRef r)
     let a' = a + delta
     invokeEvent p $
       writeRef (facilityCaptureCountRef r) a'
     invokeEvent p $
       triggerSignal (facilityCaptureCountSource r) a'

-- | Update the facility queue length and its statistics.
updateFacilityQueueCount :: MonadDES m => Facility m a -> Int -> Event m ()
{-# INLINABLE updateFacilityQueueCount #-}
updateFacilityQueueCount r delta =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (facilityQueueCountRef r)
     let a' = a + delta
     invokeEvent p $
       writeRef (facilityQueueCountRef r) a'
     invokeEvent p $
       modifyRef (facilityQueueCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (facilityQueueCountSource r) a'

-- | Update the facility utilisation count and its statistics.
updateFacilityUtilisationCount :: MonadDES m => Facility m a -> Int -> Event m ()
{-# INLINABLE updateFacilityUtilisationCount #-}
updateFacilityUtilisationCount r delta =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (facilityUtilisationCountRef r)
     let a' = a + delta
     invokeEvent p $
       writeRef (facilityUtilisationCountRef r) a'
     invokeEvent p $
       modifyRef (facilityUtilisationCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (facilityUtilisationCountSource r) a'

-- | Update the facility wait time and its statistics.
updateFacilityWaitTime :: MonadDES m => Facility m a -> Double -> Event m ()
{-# INLINABLE updateFacilityWaitTime #-}
updateFacilityWaitTime r delta =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (facilityTotalWaitTimeRef r)
     let a' = a + delta
     invokeEvent p $
       writeRef (facilityTotalWaitTimeRef r) a'
     invokeEvent p $
       modifyRef (facilityWaitTimeRef r) $
       addSamplingStats delta
     invokeEvent p $
       triggerSignal (facilityWaitTimeSource r) ()

-- | Update the facility holding time and its statistics.
updateFacilityHoldingTime :: MonadDES m => Facility m a -> Double -> Event m ()
{-# INLINABLE updateFacilityHoldingTime #-}
updateFacilityHoldingTime r delta =
  Event $ \p ->
  do a <- invokeEvent p $ readRef (facilityTotalHoldingTimeRef r)
     let a' = a + delta
     invokeEvent p $
       writeRef (facilityTotalHoldingTimeRef r) a'
     invokeEvent p $
       modifyRef (facilityHoldingTimeRef r) $
       addSamplingStats delta
     invokeEvent p $
       triggerSignal (facilityHoldingTimeSource r) ()

-- | Reset the statistics.
resetFacility :: MonadDES m => Facility m a -> Event m ()
{-# INLINABLE resetFacility #-}
resetFacility r =
  Event $ \p ->
  do let t = pointTime p
     count <- invokeEvent p $ readRef (facilityCountRef r)
     invokeEvent p $ writeRef (facilityCountStatsRef r) $
       returnTimingStats t count
     invokeEvent p $ writeRef (facilityCaptureCountRef r) 0
     utilCount <- invokeEvent p $ readRef (facilityUtilisationCountRef r)
     invokeEvent p $ writeRef (facilityUtilisationCountStatsRef r) $
       returnTimingStats t utilCount
     queueCount <- invokeEvent p $ readRef (facilityQueueCountRef r)
     invokeEvent p $ writeRef (facilityQueueCountStatsRef r) $
       returnTimingStats t queueCount
     invokeEvent p $ writeRef (facilityTotalWaitTimeRef r) 0
     invokeEvent p $ writeRef (facilityWaitTimeRef r) emptySamplingStats
     invokeEvent p $ writeRef (facilityTotalHoldingTimeRef r) 0
     invokeEvent p $ writeRef (facilityHoldingTimeRef r) emptySamplingStats
     invokeEvent p $
       triggerSignal (facilityCaptureCountSource r) 0
     invokeEvent p $
       triggerSignal (facilityWaitTimeSource r) ()
     invokeEvent p $
       triggerSignal (facilityHoldingTimeSource r) ()
