
-- |
-- Module     : Simulation.Aivika.GPSS.Results
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module allows exporting the simulation results from the model.
--
module Simulation.Aivika.GPSS.Results () where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika

import qualified Simulation.Aivika.GPSS.Queue as Q
import Simulation.Aivika.GPSS.Facility
import Simulation.Aivika.GPSS.Storage
import Simulation.Aivika.GPSS.Results.Locale
  
-- | Return a source by the specified queue.
queueResultSource :: ResultContainer Q.Queue
                     -- ^ the queue container
                     -> ResultSource
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
queueResultSummary :: ResultContainer Q.Queue
                      -- ^ the queue container
                      -> ResultSource
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
facilityResultSource :: ResultContainer (Facility a)
                        -- ^ the facility container
                        -> ResultSource
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
facilityResultSummary :: ResultContainer (Facility a)
                         -- ^ the facility container
                         -> ResultSource
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

-- | Return a source by the specified storage.
storageResultSource :: ResultContainer Storage
                       -- ^ the storage container
                       -> ResultSource
storageResultSource c =
  ResultObjectSource $
  ResultObject {
    resultObjectName = resultContainerName c,
    resultObjectId = resultContainerId c,
    resultObjectTypeId = storageId,
    resultObjectSignal = resultContainerSignal c,
    resultObjectSummary = storageResultSummary c,
    resultObjectProperties = [
      resultContainerConstProperty c "capacity" storageCapacityId storageCapacity,
      resultContainerIntegProperty c "empty" storageEmptyId storageEmpty,
      resultContainerIntegProperty c "full" storageFullId storageFull,
      resultContainerProperty c "queueCount" storageQueueCountId storageQueueCount storageQueueCountChanged_,
      resultContainerProperty c "queueCountStats" storageQueueCountStatsId storageQueueCountStats storageQueueCountChanged_,
      resultContainerProperty c "totalWaitTime" storageTotalWaitTimeId storageTotalWaitTime storageWaitTimeChanged_,
      resultContainerProperty c "waitTime" storageWaitTimeId storageWaitTime storageWaitTimeChanged_,
      resultContainerIntegProperty c "averageHoldingTime" storageAverageHoldingTimeId storageAverageHoldingTime,
      resultContainerProperty c "content" storageContentId storageContent storageContentChanged_,
      resultContainerProperty c "contentStats" storageContentStatsId storageContentStats storageContentChanged_,
      resultContainerProperty c "useCount" storageUseCountId storageUseCount storageUseCountChanged_,
      resultContainerProperty c "usedContent" storageUsedContentId storageUsedContent storageUsedContentChanged_,
      resultContainerProperty c "utilisationCount" storageUtilisationCountId storageUtilisationCount storageUtilisationCountChanged_,
      resultContainerProperty c "utilisationCountStats" storageUtilisationCountStatsId storageUtilisationCountStats storageUtilisationCountChanged_ ] }

-- | Return a summary by the specified storage.
storageResultSummary :: ResultContainer Storage
                        -- ^ the storage container
                        -> ResultSource
storageResultSummary c =
  ResultObjectSource $
  ResultObject {
    resultObjectName = resultContainerName c,
    resultObjectId = resultContainerId c,
    resultObjectTypeId = storageId,
    resultObjectSignal = resultContainerSignal c,
    resultObjectSummary = storageResultSummary c,
    resultObjectProperties = [
      resultContainerConstProperty c "capacity" storageCapacityId storageCapacity,
      resultContainerProperty c "queueCountStats" storageQueueCountStatsId storageQueueCountStats storageQueueCountChanged_,
      resultContainerProperty c "waitTime" storageWaitTimeId storageWaitTime storageWaitTimeChanged_,
      resultContainerIntegProperty c "averageHoldingTime" storageAverageHoldingTimeId storageAverageHoldingTime,
      resultContainerProperty c "contentStats" storageContentStatsId storageContentStats storageContentChanged_,
      resultContainerProperty c "useCount" storageUseCountId storageUseCount storageUseCountChanged_,
      resultContainerProperty c "usedContent" storageUsedContentId storageUsedContent storageUsedContentChanged_,
      resultContainerProperty c "utilisationCountStats" storageUtilisationCountStatsId storageUtilisationCountStats storageUtilisationCountChanged_ ] }

instance ResultProvider Q.Queue where

  resultSource' name names i is m =
    queueResultSource $ ResultContainer name names i is m (ResultSignal $ Q.queueChanged_ m)

instance ResultProvider (Facility a) where

  resultSource' name names i is m =
    facilityResultSource $ ResultContainer name names i is m (ResultSignal $ facilityChanged_ m)

instance ResultProvider Storage where

  resultSource' name names i is m =
    storageResultSource $ ResultContainer name names i is m (ResultSignal $ storageChanged_ m)
