
-- |
-- Module     : Simulation.Aivika.GPSS.Facility
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines the GPSS Facility entity.
--
module Simulation.Aivika.GPSS.Facility
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

-- | Represents a GPSS Facility entity.
data Facility a = 
  Facility { facilityCountRef :: IORef Int,
             facilityCountStatsRef :: IORef (TimingStats Int),
             facilityCountSource :: SignalSource Int,
             facilityCaptureCountRef :: IORef Int,
             facilityCaptureCountSource :: SignalSource Int,
             facilityUtilisationCountRef :: IORef Int,
             facilityUtilisationCountStatsRef :: IORef (TimingStats Int),
             facilityUtilisationCountSource :: SignalSource Int,
             facilityQueueCountRef :: IORef Int,
             facilityQueueCountStatsRef :: IORef (TimingStats Int),
             facilityQueueCountSource :: SignalSource Int,
             facilityTotalWaitTimeRef :: IORef Double,
             facilityWaitTimeRef :: IORef (SamplingStats Double),
             facilityWaitTimeSource :: SignalSource (),
             facilityTotalHoldingTimeRef :: IORef Double,
             facilityHoldingTimeRef :: IORef (SamplingStats Double),
             facilityHoldingTimeSource :: SignalSource (),
             facilityOwnerRef :: IORef (Maybe (FacilityOwnerItem a)),
             facilityDelayChain :: StrategyQueue (TransactQueueStrategy FCFS) (FacilityDelayedItem a),
             facilityInterruptChain :: StrategyQueue (TransactQueueStrategy LCFS) (FacilityInterruptedItem a),
             facilityPendingChain :: StrategyQueue (TransactQueueStrategy LCFS) (FacilityPendingItem a) }

-- | Identifies a transact item that owns the facility.
data FacilityOwnerItem a =
  FacilityOwnerItem { ownerItemTransact :: Transact a,
                      ownerItemTime :: Double,
                      ownerItemPreempting :: Bool,
                      ownerItemAccHoldingTime :: Double }

-- | Idenitifies a transact item that was delayed.
data FacilityDelayedItem a =
  FacilityDelayedItem { delayedItemTransact :: Transact a,
                        delayedItemTime :: Double,
                        delayedItemPreempting :: Bool,
                        delayedItemCont :: FrozenCont () }

-- | Idenitifies a transact item that was interrupted.
data FacilityInterruptedItem a =
  FacilityInterruptedItem { interruptedItemTransact :: Transact a,
                            interruptedItemTime :: Double,
                            interruptedItemPreempting :: Bool,
                            interruptedItemRemainingTime :: Maybe Double,
                            interruptedItemTransfer :: Maybe (FacilityPreemptTransfer a),
                            interruptedItemAccHoldingTime :: Double }

-- | Idenitifies a transact item which is pending.
data FacilityPendingItem a =
  FacilityPendingItem { pendingItemTransact :: Transact a,
                        pendingItemTime :: Double,
                        pendingItemPreempting :: Bool,
                        pendingItemCont :: FrozenCont () }

instance Eq (Facility a) where
  x == y = facilityCountRef x == facilityCountRef y  -- unique references

-- | The facility preemption mode.
data FacilityPreemptMode a =
  FacilityPreemptMode { facilityPriorityMode :: Bool,
                        -- ^ the Priority mode; otherwise, the Interrupt mode
                        facilityTransfer :: Maybe (FacilityPreemptTransfer a),
                        -- ^ where to transfer the preempted transact,
                        -- passing in the remaining time from the ADVANCE block
                        facilityRemoveMode :: Bool
                        -- ^ the Remove mode
                      }

-- | Proceed with the computation by the specified preempted transaction
-- and remaining time from the ADVANCE block.
type FacilityPreemptTransfer a = Transact a -> Maybe Double -> Process ()

-- | The default facility preemption mode.
defaultFacilityPreemptMode :: FacilityPreemptMode a
defaultFacilityPreemptMode =
  FacilityPreemptMode { facilityPriorityMode = False,
                        facilityTransfer = Nothing,
                        facilityRemoveMode = False
                      }

-- | Create a new facility.
newFacility :: Event (Facility a)
newFacility =
  Event $ \p ->
  do let r = pointRun p
         t = pointTime p
     countRef <- newIORef 1
     countStatsRef <- newIORef $ returnTimingStats t 1
     countSource <- invokeSimulation r newSignalSource
     captureCountRef <- newIORef 0
     captureCountSource <- invokeSimulation r newSignalSource
     utilCountRef <- newIORef 0
     utilCountStatsRef <- newIORef $ returnTimingStats t 0
     utilCountSource <- invokeSimulation r newSignalSource
     queueCountRef <- newIORef 0
     queueCountStatsRef <- newIORef $ returnTimingStats t 0
     queueCountSource <- invokeSimulation r newSignalSource
     totalWaitTimeRef <- newIORef 0
     waitTimeRef <- newIORef emptySamplingStats
     waitTimeSource <- invokeSimulation r newSignalSource
     totalHoldingTimeRef <- newIORef 0
     holdingTimeRef <- newIORef emptySamplingStats
     holdingTimeSource <- invokeSimulation r newSignalSource
     ownerRef <- newIORef Nothing
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
facilityCount :: Facility a -> Event Int
facilityCount r =
  Event $ \p -> readIORef (facilityCountRef r)

-- | Return the statistics for the available count of the facility.
facilityCountStats :: Facility a -> Event (TimingStats Int)
facilityCountStats r =
  Event $ \p -> readIORef (facilityCountStatsRef r)

-- | Signal triggered when the 'facilityCount' property changes.
facilityCountChanged :: Facility a -> Signal Int
facilityCountChanged r =
  publishSignal $ facilityCountSource r

-- | Signal triggered when the 'facilityCount' property changes.
facilityCountChanged_ :: Facility a -> Signal ()
facilityCountChanged_ r =
  mapSignal (const ()) $ facilityCountChanged r

-- | Return the current capture count of the facility.
facilityCaptureCount :: Facility a -> Event Int
facilityCaptureCount r =
  Event $ \p -> readIORef (facilityCaptureCountRef r)

-- | Signal triggered when the 'facilityCaptureCount' property changes.
facilityCaptureCountChanged :: Facility a -> Signal Int
facilityCaptureCountChanged r =
  publishSignal $ facilityCaptureCountSource r

-- | Signal triggered when the 'facilityCaptureCount' property changes.
facilityCaptureCountChanged_ :: Facility a -> Signal ()
facilityCaptureCountChanged_ r =
  mapSignal (const ()) $ facilityCaptureCountChanged r

-- | Return the current utilisation count of the facility.
facilityUtilisationCount :: Facility a -> Event Int
facilityUtilisationCount r =
  Event $ \p -> readIORef (facilityUtilisationCountRef r)

-- | Return the statistics for the utilisation count of the facility.
facilityUtilisationCountStats :: Facility a -> Event (TimingStats Int)
facilityUtilisationCountStats r =
  Event $ \p -> readIORef (facilityUtilisationCountStatsRef r)

-- | Signal triggered when the 'facilityUtilisationCount' property changes.
facilityUtilisationCountChanged :: Facility a -> Signal Int
facilityUtilisationCountChanged r =
  publishSignal $ facilityUtilisationCountSource r

-- | Signal triggered when the 'facilityUtilisationCount' property changes.
facilityUtilisationCountChanged_ :: Facility a -> Signal ()
facilityUtilisationCountChanged_ r =
  mapSignal (const ()) $ facilityUtilisationCountChanged r

-- | Return the current queue length of the facility.
facilityQueueCount :: Facility a -> Event Int
facilityQueueCount r =
  Event $ \p -> readIORef (facilityQueueCountRef r)

-- | Return the statistics for the queue length of the facility.
facilityQueueCountStats :: Facility a -> Event (TimingStats Int)
facilityQueueCountStats r =
  Event $ \p -> readIORef (facilityQueueCountStatsRef r)

-- | Signal triggered when the 'facilityQueueCount' property changes.
facilityQueueCountChanged :: Facility a -> Signal Int
facilityQueueCountChanged r =
  publishSignal $ facilityQueueCountSource r

-- | Signal triggered when the 'facilityQueueCount' property changes.
facilityQueueCountChanged_ :: Facility a -> Signal ()
facilityQueueCountChanged_ r =
  mapSignal (const ()) $ facilityQueueCountChanged r

-- | Return the total wait time of the facility.
facilityTotalWaitTime :: Facility a -> Event Double
facilityTotalWaitTime r =
  Event $ \p -> readIORef (facilityTotalWaitTimeRef r)

-- | Return the statistics for the wait time of the facility.
facilityWaitTime :: Facility a -> Event (SamplingStats Double)
facilityWaitTime r =
  Event $ \p -> readIORef (facilityWaitTimeRef r)

-- | Signal triggered when the 'facilityTotalWaitTime' and 'facilityWaitTime' properties change.
facilityWaitTimeChanged :: Facility a -> Signal (SamplingStats Double)
facilityWaitTimeChanged r =
  mapSignalM (\() -> facilityWaitTime r) $ facilityWaitTimeChanged_ r

-- | Signal triggered when the 'facilityTotalWaitTime' and 'facilityWaitTime' properties change.
facilityWaitTimeChanged_ :: Facility a -> Signal ()
facilityWaitTimeChanged_ r =
  publishSignal $ facilityWaitTimeSource r

-- | Return the total holding time of the facility.
facilityTotalHoldingTime :: Facility a -> Event Double
facilityTotalHoldingTime r =
  Event $ \p -> readIORef (facilityTotalHoldingTimeRef r)

-- | Return the statistics for the holding time of the facility.
facilityHoldingTime :: Facility a -> Event (SamplingStats Double)
facilityHoldingTime r =
  Event $ \p -> readIORef (facilityHoldingTimeRef r)

-- | Signal triggered when the 'facilityTotalHoldingTime' and 'facilityHoldingTime' properties change.
facilityHoldingTimeChanged :: Facility a -> Signal (SamplingStats Double)
facilityHoldingTimeChanged r =
  mapSignalM (\() -> facilityHoldingTime r) $ facilityHoldingTimeChanged_ r

-- | Signal triggered when the 'facilityTotalHoldingTime' and 'facilityHoldingTime' properties change.
facilityHoldingTimeChanged_ :: Facility a -> Signal ()
facilityHoldingTimeChanged_ r =
  publishSignal $ facilityHoldingTimeSource r

-- | Whether the facility is currently interrupted.
facilityInterrupted :: Facility a -> Event Bool
facilityInterrupted r =
  Event $ \p ->
  do x <- readIORef (facilityOwnerRef r)
     case x of
       Nothing -> return False
       Just a  -> return (ownerItemPreempting a)

-- | Seize the facility.
seizeFacility :: Facility a
                 -- ^ the requested facility
                 -> Transact a
                 -- ^ the transact that tries to seize the facility
                 -> Process ()
seizeFacility r transact =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do let t = pointTime p
     a <- readIORef (facilityOwnerRef r)
     case a of
       Nothing ->
         do writeIORef (facilityOwnerRef r) $ Just (FacilityOwnerItem transact t False 0)
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
preemptFacility :: Facility a
                   -- ^ the requested facility
                   -> Transact a
                   -- ^ the transact that tries to preempt the facility
                   -> FacilityPreemptMode a
                   -- ^ the Preempt mode
                   -> Process ()
preemptFacility r transact mode =
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do let t = pointTime p
     a <- readIORef (facilityOwnerRef r)
     case a of
       Nothing ->
         do writeIORef (facilityOwnerRef r) $ Just (FacilityOwnerItem transact t True 0)
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
         do writeIORef (facilityOwnerRef r) $ Just (FacilityOwnerItem transact t True 0)
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
         do writeIORef (facilityOwnerRef r) $ Just (FacilityOwnerItem transact t True 0)
            pid0 <- invokeEvent p $ requireTransactProcessId transact0
            t2   <- invokeEvent p $ processInterruptionTime pid0
            let dt0 = fmap (\x -> x - t) t2
            invokeEvent p $ updateFacilityWaitTime r 0
            invokeEvent p $ updateFacilityCaptureCount r 1
            invokeEvent p $ updateFacilityHoldingTime r (acc0 + (t - t0))
            case facilityTransfer mode of
              Nothing ->
                throwIO $
                SimulationRetry
                "The transfer destination is not specified for the removed preempted transact: preemptFacility"
              Just transfer ->
                invokeEvent p $ transferTransact transact0 (transfer transact0 dt0)
            invokeEvent p $ resumeCont c ()

-- | Return the facility by the active transact.
returnFacility :: Facility a
                  -- ^ the facility to return
                  -> Transact a
                  -- ^ the active transact that tries to return the facility
                  -> Process ()
returnFacility r transact = releaseFacility' r transact True 

-- | Release the facility by the active transact.
releaseFacility :: Facility a
                   -- ^ the facility to release
                   -> Transact a
                   -- ^ the active transact that tries to release the facility
                   -> Process ()
releaseFacility r transact = releaseFacility' r transact False 

-- | Release the facility by the active transact.
releaseFacility' :: Facility a
                    -- ^ the facility to release
                    -> Transact a
                    -- ^ the active transact that tries to release the facility
                    -> Bool
                    -- ^ whether the transact is preempting
                    -> Process ()
releaseFacility' r transact preempting = 
  Process $ \pid ->
  Cont $ \c ->
  Event $ \p ->
  do let t = pointTime p
     a <- readIORef (facilityOwnerRef r)
     case a of
       Nothing ->
         throwIO $
         SimulationRetry
         "There is no owner of the facility: releaseFacility'"
       Just owner@(FacilityOwnerItem transact0 t0 preempting0 acc0) | transact0 == transact && preempting0 /= preempting ->
         throwIO $
         SimulationRetry
         "The mismatch use of releaseFacility and returnFacility: releaseFacility'"
       Just owner@(FacilityOwnerItem transact0 t0 preempting0 acc0) | transact0 == transact ->
         do writeIORef (facilityOwnerRef r) Nothing
            invokeEvent p $ updateFacilityUtilisationCount r (-1)
            invokeEvent p $ updateFacilityHoldingTime r (acc0 + (t - t0))
            invokeEvent p $ releaseFacility'' r
            invokeEvent p $ resumeCont c ()
       Just owner ->
         throwIO $
         SimulationRetry
         "The facility has another owner: releaseFacility'"

-- | Find another owner of the facility.
releaseFacility'' :: Facility a -> Event ()
releaseFacility'' r =
  Event $ \p ->
  do let t = pointTime p
     f <- invokeEvent p $ strategyQueueNull (facilityPendingChain r)
     if not f
       then do FacilityPendingItem transact t0 preempting c0 <- invokeEvent p $ strategyDequeue (facilityPendingChain r)
               invokeEvent p $ updateFacilityQueueCount r (-1)
               c <- invokeEvent p $ unfreezeCont c0
               case c of
                 Nothing ->
                   invokeEvent p $ releaseFacility'' r
                 Just c ->
                   do writeIORef (facilityOwnerRef r) $ Just (FacilityOwnerItem transact t preempting 0)
                      invokeEvent p $ updateFacilityWaitTime r (t - t0)
                      invokeEvent p $ updateFacilityUtilisationCount r 1
                      invokeEvent p $ updateFacilityCaptureCount r 1
                      invokeEvent p $ enqueueEvent t $ reenterCont c ()
       else do f <- invokeEvent p $ strategyQueueNull (facilityInterruptChain r)
               if not f
                  then do FacilityInterruptedItem transact t0 preempting dt0 transfer0 acc0 <- invokeEvent p $ strategyDequeue (facilityInterruptChain r)
                          pid <- invokeEvent p $ requireTransactProcessId transact
                          invokeEvent p $ updateFacilityQueueCount r (-1)
                          f <- invokeEvent p $ processCancelled pid
                          case f of
                            True ->
                              invokeEvent p $ releaseFacility'' r
                            False ->
                              do writeIORef (facilityOwnerRef r) $ Just (FacilityOwnerItem transact t preempting acc0)
                                 invokeEvent p $ updateFacilityWaitTime r (t - t0)
                                 invokeEvent p $ updateFacilityUtilisationCount r 1
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
                                       invokeEvent p $ releaseFacility'' r
                                     Just c ->
                                       do writeIORef (facilityOwnerRef r) $ Just (FacilityOwnerItem transact t preempting 0)
                                          invokeEvent p $ updateFacilityWaitTime r (t - t0)
                                          invokeEvent p $ updateFacilityUtilisationCount r 1
                                          invokeEvent p $ updateFacilityCaptureCount r 1
                                          invokeEvent p $ enqueueEvent t $ reenterCont c ()
                           else invokeEvent p $ updateFacilityCount r 1

-- | Signal triggered when one of the facility counters changes.
facilityChanged_ :: Facility a -> Signal ()
facilityChanged_ r =
  facilityCountChanged_ r <>
  facilityCaptureCountChanged_ r <>
  facilityUtilisationCountChanged_ r <>
  facilityQueueCountChanged_ r

-- | Update the facility count and its statistics.
updateFacilityCount :: Facility a -> Int -> Event ()
updateFacilityCount r delta =
  Event $ \p ->
  do a <- readIORef (facilityCountRef r)
     let a' = a + delta
     a' `seq` writeIORef (facilityCountRef r) a'
     modifyIORef' (facilityCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (facilityCountSource r) a'

-- | Update the facility capture count.
updateFacilityCaptureCount :: Facility a -> Int -> Event ()
updateFacilityCaptureCount r delta =
  Event $ \p ->
  do a <- readIORef (facilityCaptureCountRef r)
     let a' = a + delta
     a' `seq` writeIORef (facilityCaptureCountRef r) a'
     invokeEvent p $
       triggerSignal (facilityCaptureCountSource r) a'

-- | Update the facility queue length and its statistics.
updateFacilityQueueCount :: Facility a -> Int -> Event ()
updateFacilityQueueCount r delta =
  Event $ \p ->
  do a <- readIORef (facilityQueueCountRef r)
     let a' = a + delta
     a' `seq` writeIORef (facilityQueueCountRef r) a'
     modifyIORef' (facilityQueueCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (facilityQueueCountSource r) a'

-- | Update the facility utilisation count and its statistics.
updateFacilityUtilisationCount :: Facility a -> Int -> Event ()
updateFacilityUtilisationCount r delta =
  Event $ \p ->
  do a <- readIORef (facilityUtilisationCountRef r)
     let a' = a + delta
     a' `seq` writeIORef (facilityUtilisationCountRef r) a'
     modifyIORef' (facilityUtilisationCountStatsRef r) $
       addTimingStats (pointTime p) a'
     invokeEvent p $
       triggerSignal (facilityUtilisationCountSource r) a'

-- | Update the facility wait time and its statistics.
updateFacilityWaitTime :: Facility a -> Double -> Event ()
updateFacilityWaitTime r delta =
  Event $ \p ->
  do a <- readIORef (facilityTotalWaitTimeRef r)
     let a' = a + delta
     a' `seq` writeIORef (facilityTotalWaitTimeRef r) a'
     modifyIORef' (facilityWaitTimeRef r) $
       addSamplingStats delta
     invokeEvent p $
       triggerSignal (facilityWaitTimeSource r) ()

-- | Update the facility holding time and its statistics.
updateFacilityHoldingTime :: Facility a -> Double -> Event ()
updateFacilityHoldingTime r delta =
  Event $ \p ->
  do a <- readIORef (facilityTotalHoldingTimeRef r)
     let a' = a + delta
     a' `seq` writeIORef (facilityTotalHoldingTimeRef r) a'
     modifyIORef' (facilityHoldingTimeRef r) $
       addSamplingStats delta
     invokeEvent p $
       triggerSignal (facilityHoldingTimeSource r) ()
