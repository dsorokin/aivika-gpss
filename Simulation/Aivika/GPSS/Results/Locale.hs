
-- |
-- Module     : Simulation.Aivika.GPSS.Results.Locale
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines locales for the simulation results.
--
module Simulation.Aivika.GPSS.Results.Locale where

import qualified Data.Map as M

import Simulation.Aivika

import qualified Simulation.Aivika.GPSS.Queue as Q
import Simulation.Aivika.GPSS.Facility
import Simulation.Aivika.GPSS.Storage

-- | The queue identifier.
queueId :: ResultId
queueId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "очередь"),
   (englishResultLocale, "the queue")]

-- | Property 'Q.queueNull'.
queueNullId :: ResultId
queueNullId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "очередь пуста?"),
   (englishResultLocale, "is the queue empty?")]

-- | Property 'Q.queueContent'.
queueContentId :: ResultId
queueContentId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "текущее содержимое очереди"),
   (englishResultLocale, "the current queue content")]

-- | Property 'Q.queueContentStats'.
queueContentStatsId :: ResultId
queueContentStatsId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "статистика по содержимому очереди"),
   (englishResultLocale, "the queue content statistics")]

-- | Property 'Q.enqueueCount'.
enqueueCountId :: ResultId
enqueueCountId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "количество добавленных транзактов"),
   (englishResultLocale, "the number of enqueued transacts")]

-- | Property 'Q.enqueueZeroEntryCount'.
enqueueZeroEntryCountId :: ResultId
enqueueZeroEntryCountId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "количество добавленных транзактов без фактического ожидания"),
   (englishResultLocale, "the number of zero entry enqueued transacts")]

-- | Property 'Q.queueWaitTime'.
queueWaitTimeId :: ResultId
queueWaitTimeId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "время ожидания"),
   (englishResultLocale, "the wait time")]

-- | Property 'Q.queueNonZeroEntryWaitTime'.
queueNonZeroEntryWaitTimeId :: ResultId
queueNonZeroEntryWaitTimeId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "время ожидания без учета фактически неожидавших транзактов"),
   (englishResultLocale, "the wait time without zero entries")]

-- | Property 'Q.queueRate'.
queueRateId :: ResultId
queueRateId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "усредненная скорость (как средняя длина очереди на среднее время ожидания)"),
   (englishResultLocale, "the average queue rate (= queue size / wait time)")]

-- | The facility identifier.
facilityId :: ResultId
facilityId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "прибор"),
   (englishResultLocale, "the facility")]

-- | Property 'facilityCount'.
facilityCountId :: ResultId
facilityCountId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "текущее доступное количество прибора"),
   (englishResultLocale, "the current available count")]

-- | Property 'facilityCountStats'.
facilityCountStatsId :: ResultId
facilityCountStatsId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "статистика доступного количества прибора"),
   (englishResultLocale, "the available count statistics")]

-- | Property 'facilityCaptureCount'.
facilityCaptureCountId :: ResultId
facilityCaptureCountId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "текущее количество захвата прибора"),
   (englishResultLocale, "the current capture count")]

-- | Property 'facilityUtilisationCount'.
facilityUtilisationCountId :: ResultId
facilityUtilisationCountId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "текущее используемое количество прибора"),
   (englishResultLocale, "the current utilisation count")]

-- | Property 'facilityUtilisationCountStats'.
facilityUtilisationCountStatsId :: ResultId
facilityUtilisationCountStatsId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "статистка по используемому количеству прибора"),
   (englishResultLocale, "the utilisation count statistics")]

-- | Property 'facilityQueueCount'.
facilityQueueCountId :: ResultId
facilityQueueCountId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "текущая длина очереди к прибору"),
   (englishResultLocale, "the current queue size")]

-- | Property 'facilityQueueCountStats'.
facilityQueueCountStatsId :: ResultId
facilityQueueCountStatsId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "статистика длины очереди к прибору"),
   (englishResultLocale, "the queue size statistics")]

-- | Property 'facilityTotalWaitTime'.
facilityTotalWaitTimeId :: ResultId
facilityTotalWaitTimeId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "общее время ожидания прибора"),
   (englishResultLocale, "the total wait time")]

-- | Property 'facilityWaitTime'.
facilityWaitTimeId :: ResultId
facilityWaitTimeId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "время ожидания прибора"),
   (englishResultLocale, "the wait time")]

-- | Property 'facilityTotalHoldingTime'.
facilityTotalHoldingTimeId :: ResultId
facilityTotalHoldingTimeId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "общее время удержания прибора"),
   (englishResultLocale, "the total holding time")]

-- | Property 'facilityHoldingTime'.
facilityHoldingTimeId :: ResultId
facilityHoldingTimeId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "время удержания прибора"),
   (englishResultLocale, "the holding time")]

-- | Property 'facilityInterrupted'.
facilityInterruptedId :: ResultId
facilityInterruptedId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "удержание прибора сейчас прервано?"),
   (englishResultLocale, "is the facility interrupted now?")]

-- | The storage identifier.
storageId :: ResultId
storageId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "многоканальное устройство"),
   (englishResultLocale, "the storage")]

-- | Property 'storageCapacity'.
storageCapacityId :: ResultId
storageCapacityId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "емкость многоканального устройства"),
   (englishResultLocale, "the storage capacity")]

-- | Property 'storageEmpty'.
storageEmptyId :: ResultId
storageEmptyId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "не используется ли совсем устройство сейчас?"),
   (englishResultLocale, "is the storage unused completely now?")]

-- | Property 'storageFull'.
storageFullId :: ResultId
storageFullId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "используется ли устройство полностью сейчас?"),
   (englishResultLocale, "is the storage used completely now?")]

-- | Property 'storageContent'.
storageContentId :: ResultId
storageContentId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "доступное содержимое устройства"),
   (englishResultLocale, "the current available content")]

-- | Property 'storageContentStats'.
storageContentStatsId :: ResultId
storageContentStatsId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "статистика доступного содержимого устройства"),
   (englishResultLocale, "the available content statistics")]

-- | Property 'storageUseCount'.
storageUseCountId :: ResultId
storageUseCountId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "общее количество использований устройства"),
   (englishResultLocale, "the total use count")]

-- | Property 'storageUsedContent'.
storageUsedContentId :: ResultId
storageUsedContentId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "общее использованное количество устройства"),
   (englishResultLocale, "the total used content")]

-- | Property 'storageUtilisationCount'.
storageUtilisationCountId :: ResultId
storageUtilisationCountId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "текущее используемое количество устройства"),
   (englishResultLocale, "the current utilisation count")]

-- | Property 'storageUtilisationCountStats'.
storageUtilisationCountStatsId :: ResultId
storageUtilisationCountStatsId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "статистка по используемому количеству устройства"),
   (englishResultLocale, "the utilisation count statistics")]

-- | Property 'storageQueueCount'.
storageQueueCountId :: ResultId
storageQueueCountId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "текущая длина очереди к устройству"),
   (englishResultLocale, "the current queue size")]

-- | Property 'storageQueueCountStats'.
storageQueueCountStatsId :: ResultId
storageQueueCountStatsId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "статистика длины очереди к устройству"),
   (englishResultLocale, "the queue size statistics")]

-- | Property 'storageTotalWaitTime'.
storageTotalWaitTimeId :: ResultId
storageTotalWaitTimeId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "общее время ожидания устройства"),
   (englishResultLocale, "the total wait time")]

-- | Property 'storageWaitTime'.
storageWaitTimeId :: ResultId
storageWaitTimeId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "время ожидания устройства"),
   (englishResultLocale, "the wait time")]

-- | Property 'storageAverageHoldingTime'.
storageAverageHoldingTimeId :: ResultId
storageAverageHoldingTimeId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "среднее время удержания устройства на единицу устройства"),
   (englishResultLocale, "the average holding time per unit")]
