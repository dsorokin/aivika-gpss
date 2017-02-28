
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Results
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module allows exporting the simulation results from the model.
--
module Simulation.Aivika.Trans.GPSS.Results () where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans

import qualified Simulation.Aivika.Trans.GPSS.Queue as Q
import Simulation.Aivika.Trans.GPSS.Facility
import Simulation.Aivika.Trans.GPSS.Results.Locale
  
-- | Return a source by the specified queue.
queueResultSource :: MonadDES m
                     => ResultContainer (Q.Queue m) m
                     -- ^ the queue container
                     -> ResultSource m
queueResultSource c =
  ResultObjectSource $
  ResultObject {
    resultObjectName = resultContainerName c,
    resultObjectId = resultContainerId c,
    resultObjectTypeId = queueId,
    resultObjectSignal = resultContainerSignal c,
    resultObjectSummary = queueResultSummary c,
    resultObjectProperties = [
      resultContainerProperty c "queueNull" queueNullId Q.queueNull Q.queueNullChanged_,
      resultContainerProperty c "queueContent" queueContentId Q.queueContent Q.queueContentChanged_,
      resultContainerProperty c "queueContentStats" queueContentStatsId Q.queueContentStats Q.queueContentChanged_,
      resultContainerProperty c "enqueueCount" enqueueCountId Q.enqueueCount Q.enqueueCountChanged_,
      resultContainerProperty c "enqueueZeroEntryCount" enqueueZeroEntryCountId Q.enqueueZeroEntryCount Q.enqueueZeroEntryCountChanged_,
      resultContainerProperty c "queueWaitTime" queueWaitTimeId Q.queueWaitTime Q.queueWaitTimeChanged_,
      resultContainerProperty c "queueNonZeroEntryWaitTime" queueNonZeroEntryWaitTimeId Q.queueNonZeroEntryWaitTime Q.queueNonZeroEntryWaitTimeChanged_,
      resultContainerProperty c "queueRate" queueRateId Q.queueRate Q.queueRateChanged_ ] }

-- | Return the summary by the specified queue.
queueResultSummary :: MonadDES m =>
                      ResultContainer (Q.Queue m) m
                      -- ^ the queue container
                      -> ResultSource m
queueResultSummary c =
  ResultObjectSource $
  ResultObject {
    resultObjectName = resultContainerName c,
    resultObjectId = resultContainerId c,
    resultObjectTypeId = queueId,
    resultObjectSignal = resultContainerSignal c,
    resultObjectSummary = queueResultSummary c,
    resultObjectProperties = [
      resultContainerProperty c "queueContentStats" queueContentStatsId Q.queueContentStats Q.queueContentChanged_,
      resultContainerProperty c "enqueueCount" enqueueCountId Q.enqueueCount Q.enqueueCountChanged_,
      resultContainerProperty c "enqueueZeroEntryCount" enqueueZeroEntryCountId Q.enqueueZeroEntryCount Q.enqueueZeroEntryCountChanged_,
      resultContainerProperty c "queueWaitTime" queueWaitTimeId Q.queueWaitTime Q.queueWaitTimeChanged_,
      resultContainerProperty c "queueNonZeroEntryWaitTime" queueNonZeroEntryWaitTimeId Q.queueNonZeroEntryWaitTime Q.queueNonZeroEntryWaitTimeChanged_,
      resultContainerProperty c "queueRate" queueRateId Q.queueRate Q.queueRateChanged_ ] }

-- | Return a source by the specified facility.
facilityResultSource :: MonadDES m =>
                        ResultContainer (Facility m a) m
                        -- ^ the facility container
                        -> ResultSource m
facilityResultSource c =
  ResultObjectSource $
  ResultObject {
    resultObjectName = resultContainerName c,
    resultObjectId = resultContainerId c,
    resultObjectTypeId = facilityId,
    resultObjectSignal = resultContainerSignal c,
    resultObjectSummary = facilityResultSummary c,
    resultObjectProperties = [
      resultContainerProperty c "queueCount" facilityQueueCountId facilityQueueCount facilityQueueCountChanged_,
      resultContainerProperty c "queueCountStats" facilityQueueCountStatsId facilityQueueCountStats facilityQueueCountChanged_,
      resultContainerProperty c "totalWaitTime" facilityTotalWaitTimeId facilityTotalWaitTime facilityWaitTimeChanged_,
      resultContainerProperty c "waitTime" facilityWaitTimeId facilityWaitTime facilityWaitTimeChanged_,
      resultContainerProperty c "totalHoldingTime" facilityTotalHoldingTimeId facilityTotalHoldingTime facilityHoldingTimeChanged_,
      resultContainerProperty c "holdingTime" facilityHoldingTimeId facilityHoldingTime facilityHoldingTimeChanged_,
      resultContainerIntegProperty c "interrupted" facilityInterruptedId facilityInterrupted,
      resultContainerProperty c "count" facilityCountId facilityCount facilityCountChanged_,
      resultContainerProperty c "countStats" facilityCountStatsId facilityCountStats facilityCountChanged_,
      resultContainerProperty c "captureCount" facilityCaptureCountId facilityCaptureCount facilityCaptureCountChanged_,
      resultContainerProperty c "utilisationCount" facilityUtilisationCountId facilityUtilisationCount facilityUtilisationCountChanged_,
      resultContainerProperty c "utilisationCountStats" facilityUtilisationCountStatsId facilityUtilisationCountStats facilityUtilisationCountChanged_ ] }

-- | Return a summary by the specified facility.
facilityResultSummary :: MonadDES m =>
                         ResultContainer (Facility m a) m
                         -- ^ the facility container
                         -> ResultSource m
facilityResultSummary c =
  ResultObjectSource $
  ResultObject {
    resultObjectName = resultContainerName c,
    resultObjectId = resultContainerId c,
    resultObjectTypeId = facilityId,
    resultObjectSignal = resultContainerSignal c,
    resultObjectSummary = facilityResultSummary c,
    resultObjectProperties = [
      resultContainerProperty c "queueCountStats" facilityQueueCountStatsId facilityQueueCountStats facilityQueueCountChanged_,
      resultContainerProperty c "waitTime" facilityWaitTimeId facilityWaitTime facilityWaitTimeChanged_,
      resultContainerProperty c "holdingTime" facilityHoldingTimeId facilityHoldingTime facilityHoldingTimeChanged_,
      resultContainerProperty c "countStats" facilityCountStatsId facilityCountStats facilityCountChanged_,
      resultContainerProperty c "captureCount" facilityCaptureCountId facilityCaptureCount facilityCaptureCountChanged_,
      resultContainerProperty c "utilisationCountStats" facilityUtilisationCountStatsId facilityUtilisationCountStats facilityUtilisationCountChanged_ ] }

instance MonadDES m => ResultProvider (Q.Queue m) m where

  resultSource' name i m =
    queueResultSource $ ResultContainer name i m (ResultSignal $ Q.queueChanged_ m)

instance MonadDES m => ResultProvider (Facility m a) m where

  resultSource' name i m =
    facilityResultSource $ ResultContainer name i m (ResultSignal $ facilityChanged_ m)