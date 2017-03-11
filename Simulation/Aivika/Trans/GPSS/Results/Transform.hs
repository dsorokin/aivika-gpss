
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Results.Transform
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines transformations for the simulation results.
--
module Simulation.Aivika.Trans.GPSS.Results.Transform where

import Control.Category

import Simulation.Aivika.Trans
import qualified Simulation.Aivika.Trans.Results.Transform as T

import qualified Simulation.Aivika.Trans.GPSS.Queue as Q
import qualified Simulation.Aivika.Trans.GPSS.Facility as F
import qualified Simulation.Aivika.Trans.GPSS.Storage as S
import Simulation.Aivika.Trans.GPSS.Results
import Simulation.Aivika.Trans.GPSS.Results.Locale

-- | Represents the 'Q.Queue'.
newtype Queue m = Queue (ResultTransform m)

-- | An instance of the result transformer.
instance T.ResultTransformer (Queue m) m where
  tr (Queue a) = a

-- | Property 'Q.queueNull'.
queueNull :: Queue m -> ResultTransform m
queueNull (Queue a) =
  a >>> resultById queueNullId

-- | Property 'Q.queueContent'.
queueContent :: Queue m -> ResultTransform m
queueContent (Queue a) =
  a >>> resultById queueContentId

-- | Property 'Q.queueContentStats'.
queueContentStats :: Queue m -> T.TimingStats m
queueContentStats (Queue a) =
  T.TimingStats (a >>> resultById queueContentStatsId)

-- | Property 'Q.enqueueCount'.
enqueueCount :: Queue m -> ResultTransform m
enqueueCount (Queue a) =
  a >>> resultById enqueueCountId

-- | Property 'Q.enqueueZeroEntryCount'.
enqueueZeroEntryCount :: Queue m -> ResultTransform m
enqueueZeroEntryCount (Queue a) =
  a >>> resultById enqueueZeroEntryCountId

-- | Property 'Q.queueWaitTime'.
queueWaitTime :: Queue m -> T.SamplingStats m
queueWaitTime (Queue a) =
  T.SamplingStats (a >>> resultById queueWaitTimeId)

-- | Property 'Q.queueNonZeroEntryWaitTime'.
queueNonZeroEntryWaitTime :: Queue m -> T.SamplingStats m
queueNonZeroEntryWaitTime (Queue a) =
  T.SamplingStats (a >>> resultById queueNonZeroEntryWaitTimeId)

-- | Property 'Q.queueRate'.
queueRate :: Queue m -> ResultTransform m
queueRate (Queue a) =
  a >>> resultById queueRateId

-- | Represents the 'F.Facility'.
newtype Facility m = Facility (ResultTransform m)

-- | An instance of the result transformer.
instance T.ResultTransformer (Facility m) m where
  tr (Facility a) = a

-- | Property 'F.facilityCount'.
facilityCount :: Facility m -> ResultTransform m
facilityCount (Facility a) =
  a >>> resultById facilityCountId

-- | Property 'F.facilityCountStats'.
facilityCountStats :: Facility m -> T.TimingStats m
facilityCountStats (Facility a) =
  T.TimingStats (a >>> resultById facilityCountStatsId)

-- | Property 'F.facilityCaptureCount'.
facilityCaptureCount :: Facility m -> ResultTransform m
facilityCaptureCount (Facility a) =
  a >>> resultById facilityCaptureCountId

-- | Property 'F.facilityUtilisationCount'.
facilityUtilisationCount :: Facility m -> ResultTransform m
facilityUtilisationCount (Facility a) =
  a >>> resultById facilityUtilisationCountId

-- | Property 'F.facilityUtilisationCountStats'.
facilityUtilisationCountStats :: Facility m -> T.TimingStats m
facilityUtilisationCountStats (Facility a) =
  T.TimingStats (a >>> resultById facilityUtilisationCountStatsId)

-- | Property 'F.facilityQueueCount'.
facilityQueueCount :: Facility m -> ResultTransform m
facilityQueueCount (Facility a) =
  a >>> resultById facilityQueueCountId

-- | Property 'F.facilityQueueCountStats'.
facilityQueueCountStats :: Facility m -> T.TimingStats m
facilityQueueCountStats (Facility a) =
  T.TimingStats (a >>> resultById facilityQueueCountStatsId)

-- | Property 'F.facilityTotalWaitTime'.
facilityTotalWaitTime :: Facility m -> ResultTransform m
facilityTotalWaitTime (Facility a) =
  a >>> resultById facilityTotalWaitTimeId

-- | Property 'F.facilityWaitTime'.
facilityWaitTime :: Facility m -> T.SamplingStats m
facilityWaitTime (Facility a) =
  T.SamplingStats (a >>> resultById facilityWaitTimeId)

-- | Property 'F.facilityTotalHoldingTime'.
facilityTotalHoldingTime :: Facility m -> ResultTransform m
facilityTotalHoldingTime (Facility a) =
  a >>> resultById facilityTotalHoldingTimeId

-- | Property 'F.facilityHoldingTime'.
facilityHoldingTime :: Facility m -> T.SamplingStats m
facilityHoldingTime (Facility a) =
  T.SamplingStats (a >>> resultById facilityHoldingTimeId)

-- | Property 'F.facilityInterrupted'.
facilityInterrupted :: Facility m -> ResultTransform m
facilityInterrupted (Facility a) =
  a >>> resultById facilityInterruptedId

-- | Represents the 'S.Storage'.
newtype Storage m = Storage (ResultTransform m)

-- | An instance of the result transformer.
instance T.ResultTransformer (Storage m) m where
  tr (Storage a) = a

-- | Property 'S.storageCapacity'.
storageCapacity :: Storage m -> ResultTransform m
storageCapacity (Storage a) =
  a >>> resultById storageCapacityId

-- | Property 'S.storageEmpty'.
storageEmpty :: Storage m -> ResultTransform m
storageEmpty (Storage a) =
  a >>> resultById storageEmptyId

-- | Property 'S.storageFull'.
storageFull :: Storage m -> ResultTransform m
storageFull (Storage a) =
  a >>> resultById storageFullId

-- | Property 'S.storageContent'.
storageContent :: Storage m -> ResultTransform m
storageContent (Storage a) =
  a >>> resultById storageContentId

-- | Property 'S.storageContentStats'.
storageContentStats :: Storage m -> T.TimingStats m
storageContentStats (Storage a) =
  T.TimingStats (a >>> resultById storageContentStatsId)

-- | Property 'S.storageUseCount'.
storageUseCount :: Storage m -> ResultTransform m
storageUseCount (Storage a) =
  a >>> resultById storageUseCountId

-- | Property 'S.storageUsedContent'.
storageUsedContent :: Storage m -> ResultTransform m
storageUsedContent (Storage a) =
  a >>> resultById storageUsedContentId

-- | Property 'S.storageUtilisationCount'.
storageUtilisationCount :: Storage m -> ResultTransform m
storageUtilisationCount (Storage a) =
  a >>> resultById storageUtilisationCountId

-- | Property 'S.storageUtilisationCountStats'.
storageUtilisationCountStats :: Storage m -> T.TimingStats m
storageUtilisationCountStats (Storage a) =
  T.TimingStats (a >>> resultById storageUtilisationCountStatsId)

-- | Property 'S.storageQueueCount'.
storageQueueCount :: Storage m -> ResultTransform m
storageQueueCount (Storage a) =
  a >>> resultById storageQueueCountId

-- | Property 'S.storageQueueCountStats'.
storageQueueCountStats :: Storage m -> T.TimingStats m
storageQueueCountStats (Storage a) =
  T.TimingStats (a >>> resultById storageQueueCountStatsId)

-- | Property 'S.storageTotalWaitTime'.
storageTotalWaitTime :: Storage m -> ResultTransform m
storageTotalWaitTime (Storage a) =
  a >>> resultById storageTotalWaitTimeId

-- | Property 'S.storageWaitTime'.
storageWaitTime :: Storage m -> T.SamplingStats m
storageWaitTime (Storage a) =
  T.SamplingStats (a >>> resultById storageWaitTimeId)

-- | Property 'S.storageAverageHoldingTime'.
storageAverageHoldingTime :: Storage m -> ResultTransform m
storageAverageHoldingTime (Storage a) =
  a >>> resultById storageAverageHoldingTimeId
