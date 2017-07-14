
-- |
-- Module     : Simulation.Aivika.GPSS.Results.Transform
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines transformations for the simulation results.
--
module Simulation.Aivika.GPSS.Results.Transform where

import Control.Category

import Simulation.Aivika
import qualified Simulation.Aivika.Results.Transform as T

import qualified Simulation.Aivika.GPSS.Queue as Q
import qualified Simulation.Aivika.GPSS.Facility as F
import qualified Simulation.Aivika.GPSS.Storage as S
import Simulation.Aivika.GPSS.Results
import Simulation.Aivika.GPSS.Results.Locale

-- | Represents the 'Q.Queue'.
newtype Queue = Queue ResultTransform

-- | An instance of the result transformer.
instance T.ResultTransformer Queue where
  tr (Queue a) = a

-- | Property 'Q.queueNull'.
queueNull :: Queue -> ResultTransform
queueNull (Queue a) =
  a >>> resultById queueNullId

-- | Property 'Q.queueContent'.
queueContent :: Queue -> ResultTransform
queueContent (Queue a) =
  a >>> resultById queueContentId

-- | Property 'Q.queueContentStats'.
queueContentStats :: Queue -> T.TimingStats
queueContentStats (Queue a) =
  T.TimingStats (a >>> resultById queueContentStatsId)

-- | Property 'Q.enqueueCount'.
enqueueCount :: Queue -> ResultTransform
enqueueCount (Queue a) =
  a >>> resultById enqueueCountId

-- | Property 'Q.enqueueZeroEntryCount'.
enqueueZeroEntryCount :: Queue -> ResultTransform
enqueueZeroEntryCount (Queue a) =
  a >>> resultById enqueueZeroEntryCountId

-- | Property 'Q.queueWaitTime'.
queueWaitTime :: Queue -> T.SamplingStats
queueWaitTime (Queue a) =
  T.SamplingStats (a >>> resultById queueWaitTimeId)

-- | Property 'Q.queueNonZeroEntryWaitTime'.
queueNonZeroEntryWaitTime :: Queue -> T.SamplingStats
queueNonZeroEntryWaitTime (Queue a) =
  T.SamplingStats (a >>> resultById queueNonZeroEntryWaitTimeId)

-- | Property 'Q.queueRate'.
queueRate :: Queue -> ResultTransform
queueRate (Queue a) =
  a >>> resultById queueRateId

-- | Represents the 'F.Facility'.
newtype Facility = Facility ResultTransform

-- | An instance of the result transformer.
instance T.ResultTransformer Facility where
  tr (Facility a) = a

-- | Property 'F.facilityCount'.
facilityCount :: Facility -> ResultTransform
facilityCount (Facility a) =
  a >>> resultById facilityCountId

-- | Property 'F.facilityCountStats'.
facilityCountStats :: Facility -> T.TimingStats
facilityCountStats (Facility a) =
  T.TimingStats (a >>> resultById facilityCountStatsId)

-- | Property 'F.facilityCaptureCount'.
facilityCaptureCount :: Facility -> ResultTransform
facilityCaptureCount (Facility a) =
  a >>> resultById facilityCaptureCountId

-- | Property 'F.facilityUtilisationCount'.
facilityUtilisationCount :: Facility -> ResultTransform
facilityUtilisationCount (Facility a) =
  a >>> resultById facilityUtilisationCountId

-- | Property 'F.facilityUtilisationCountStats'.
facilityUtilisationCountStats :: Facility -> T.TimingStats
facilityUtilisationCountStats (Facility a) =
  T.TimingStats (a >>> resultById facilityUtilisationCountStatsId)

-- | Property 'F.facilityQueueCount'.
facilityQueueCount :: Facility -> ResultTransform
facilityQueueCount (Facility a) =
  a >>> resultById facilityQueueCountId

-- | Property 'F.facilityQueueCountStats'.
facilityQueueCountStats :: Facility -> T.TimingStats
facilityQueueCountStats (Facility a) =
  T.TimingStats (a >>> resultById facilityQueueCountStatsId)

-- | Property 'F.facilityTotalWaitTime'.
facilityTotalWaitTime :: Facility -> ResultTransform
facilityTotalWaitTime (Facility a) =
  a >>> resultById facilityTotalWaitTimeId

-- | Property 'F.facilityWaitTime'.
facilityWaitTime :: Facility -> T.SamplingStats
facilityWaitTime (Facility a) =
  T.SamplingStats (a >>> resultById facilityWaitTimeId)

-- | Property 'F.facilityTotalHoldingTime'.
facilityTotalHoldingTime :: Facility -> ResultTransform
facilityTotalHoldingTime (Facility a) =
  a >>> resultById facilityTotalHoldingTimeId

-- | Property 'F.facilityHoldingTime'.
facilityHoldingTime :: Facility -> T.SamplingStats
facilityHoldingTime (Facility a) =
  T.SamplingStats (a >>> resultById facilityHoldingTimeId)

-- | Property 'F.facilityInterrupted'.
facilityInterrupted :: Facility -> ResultTransform
facilityInterrupted (Facility a) =
  a >>> resultById facilityInterruptedId

-- | Represents the 'S.Storage'.
newtype Storage = Storage ResultTransform

-- | An instance of the result transformer.
instance T.ResultTransformer Storage where
  tr (Storage a) = a

-- | Property 'S.storageCapacity'.
storageCapacity :: Storage -> ResultTransform
storageCapacity (Storage a) =
  a >>> resultById storageCapacityId

-- | Property 'S.storageEmpty'.
storageEmpty :: Storage -> ResultTransform
storageEmpty (Storage a) =
  a >>> resultById storageEmptyId

-- | Property 'S.storageFull'.
storageFull :: Storage -> ResultTransform
storageFull (Storage a) =
  a >>> resultById storageFullId

-- | Property 'S.storageContent'.
storageContent :: Storage -> ResultTransform
storageContent (Storage a) =
  a >>> resultById storageContentId

-- | Property 'S.storageContentStats'.
storageContentStats :: Storage -> T.TimingStats
storageContentStats (Storage a) =
  T.TimingStats (a >>> resultById storageContentStatsId)

-- | Property 'S.storageUseCount'.
storageUseCount :: Storage -> ResultTransform
storageUseCount (Storage a) =
  a >>> resultById storageUseCountId

-- | Property 'S.storageUsedContent'.
storageUsedContent :: Storage -> ResultTransform
storageUsedContent (Storage a) =
  a >>> resultById storageUsedContentId

-- | Property 'S.storageUtilisationCount'.
storageUtilisationCount :: Storage -> ResultTransform
storageUtilisationCount (Storage a) =
  a >>> resultById storageUtilisationCountId

-- | Property 'S.storageUtilisationCountStats'.
storageUtilisationCountStats :: Storage -> T.TimingStats
storageUtilisationCountStats (Storage a) =
  T.TimingStats (a >>> resultById storageUtilisationCountStatsId)

-- | Property 'S.storageQueueCount'.
storageQueueCount :: Storage -> ResultTransform
storageQueueCount (Storage a) =
  a >>> resultById storageQueueCountId

-- | Property 'S.storageQueueCountStats'.
storageQueueCountStats :: Storage -> T.TimingStats
storageQueueCountStats (Storage a) =
  T.TimingStats (a >>> resultById storageQueueCountStatsId)

-- | Property 'S.storageTotalWaitTime'.
storageTotalWaitTime :: Storage -> ResultTransform
storageTotalWaitTime (Storage a) =
  a >>> resultById storageTotalWaitTimeId

-- | Property 'S.storageWaitTime'.
storageWaitTime :: Storage -> T.SamplingStats
storageWaitTime (Storage a) =
  T.SamplingStats (a >>> resultById storageWaitTimeId)

-- | Property 'S.storageAverageHoldingTime'.
storageAverageHoldingTime :: Storage -> ResultTransform
storageAverageHoldingTime (Storage a) =
  a >>> resultById storageAverageHoldingTimeId
