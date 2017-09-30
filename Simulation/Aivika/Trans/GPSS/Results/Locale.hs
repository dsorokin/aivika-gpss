
-- |
-- Module     : Simulation.Aivika.GPSS.Trans.Results.Locale
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines locales for the simulation results.
--
module Simulation.Aivika.Trans.GPSS.Results.Locale where

import qualified Data.Map as M

import Simulation.Aivika.Trans

import qualified Simulation.Aivika.Trans.GPSS.Queue as Q
import Simulation.Aivika.Trans.GPSS.Facility
import Simulation.Aivika.Trans.GPSS.Storage

-- | The queue identifier.
queueId :: ResultId
queueId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "очередь"),
           (englishResultLocale, "the queue")]
        titles =
          M.fromList
          [(russianResultLocale, "очередь"),
           (englishResultLocale, "queue")]

-- | Property 'Q.queueNull'.
queueNullId :: ResultId
queueNullId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "очередь пуста?"),
           (englishResultLocale, "is the queue empty?")]
        titles =
          M.fromList
          [(russianResultLocale, "очередь пуста?"),
           (englishResultLocale, "empty queue?")]

-- | Property 'Q.queueContent'.
queueContentId :: ResultId
queueContentId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "текущее содержимое очереди"),
           (englishResultLocale, "the current queue content")]
        titles =
          M.fromList
          [(russianResultLocale, "содержимое очереди"),
           (englishResultLocale, "queue content")]

-- | Property 'Q.queueContentStats'.
queueContentStatsId :: ResultId
queueContentStatsId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "статистика по содержимому очереди"),
           (englishResultLocale, "the queue content statistics")]
        titles =
          M.fromList
          [(russianResultLocale, "стат. по содержимому очереди"),
           (englishResultLocale, "queue content stats")]

-- | Property 'Q.enqueueCount'.
enqueueCountId :: ResultId
enqueueCountId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "количество добавленных транзактов"),
           (englishResultLocale, "the number of enqueued transacts")]
        titles =
          M.fromList
          [(russianResultLocale, "кол-во добавлений"),
           (englishResultLocale, "enqueue count")]

-- | Property 'Q.enqueueZeroEntryCount'.
enqueueZeroEntryCountId :: ResultId
enqueueZeroEntryCountId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "количество добавленных транзактов без фактического ожидания"),
           (englishResultLocale, "the number of zero entry enqueued transacts")]
        titles =
          M.fromList
          [(russianResultLocale, "кол-во добавлений без ожидания"),
           (englishResultLocale, "zero entry enqueue count")]

-- | Property 'Q.queueWaitTime'.
queueWaitTimeId :: ResultId
queueWaitTimeId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "время ожидания"),
           (englishResultLocale, "the wait time")]
        titles =
          M.fromList
          [(russianResultLocale, "время ожидания"),
           (englishResultLocale, "wait time")]

-- | Property 'Q.queueNonZeroEntryWaitTime'.
queueNonZeroEntryWaitTimeId :: ResultId
queueNonZeroEntryWaitTimeId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "время ожидания без учета фактически неожидавших транзактов"),
           (englishResultLocale, "the wait time without zero entries")]
        titles =
          M.fromList
          [(russianResultLocale, "время ожидания за вычетом неожидавших"),
           (englishResultLocale, "non-zero entry wait time")]

-- | Property 'Q.queueRate'.
queueRateId :: ResultId
queueRateId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "усредненная скорость (как средняя длина очереди на среднее время ожидания)"),
           (englishResultLocale, "the average queue rate (= queue size / wait time)")]
        titles =
          M.fromList
          [(russianResultLocale, "усредненная скорость"),
           (englishResultLocale, "queue rate")]

-- | The facility identifier.
facilityId :: ResultId
facilityId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "прибор"),
           (englishResultLocale, "the facility")]
        titles =
          M.fromList
          [(russianResultLocale, "прибор"),
           (englishResultLocale, "facility")]

-- | Property 'facilityCount'.
facilityCountId :: ResultId
facilityCountId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "текущее доступное количество прибора"),
           (englishResultLocale, "the current available count")]
        titles =
          M.fromList
          [(russianResultLocale, "доступное кол-во"),
           (englishResultLocale, "available count")]

-- | Property 'facilityCountStats'.
facilityCountStatsId :: ResultId
facilityCountStatsId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "статистика доступного количества прибора"),
           (englishResultLocale, "the available count statistics")]
        titles =
          M.fromList
          [(russianResultLocale, "стат. доступного кол-ва"),
           (englishResultLocale, "available count stats")]

-- | Property 'facilityCaptureCount'.
facilityCaptureCountId :: ResultId
facilityCaptureCountId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "текущее количество захватов прибора"),
           (englishResultLocale, "the current capture count")]
        titles =
          M.fromList
          [(russianResultLocale, "кол-во захватов"),
           (englishResultLocale, "capture count")]

-- | Property 'facilityUtilisationCount'.
facilityUtilisationCountId :: ResultId
facilityUtilisationCountId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "текущее используемое количество прибора"),
           (englishResultLocale, "the current utilisation count")]
        titles =
          M.fromList
          [(russianResultLocale, "используемое кол-во"),
           (englishResultLocale, "utilisation count")]

-- | Property 'facilityUtilisationCountStats'.
facilityUtilisationCountStatsId :: ResultId
facilityUtilisationCountStatsId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "статистка по используемому количеству прибора"),
           (englishResultLocale, "the utilisation count statistics")]
        titles =
          M.fromList
          [(russianResultLocale, "стат. по используемому кол-ву"),
           (englishResultLocale, "utilisation count stats")]

-- | Property 'facilityQueueCount'.
facilityQueueCountId :: ResultId
facilityQueueCountId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "текущая длина очереди к прибору"),
           (englishResultLocale, "the current queue size")]
        titles =
          M.fromList
          [(russianResultLocale, "длина очереди"),
           (englishResultLocale, "queue size")]

-- | Property 'facilityQueueCountStats'.
facilityQueueCountStatsId :: ResultId
facilityQueueCountStatsId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "статистика длины очереди к прибору"),
           (englishResultLocale, "the queue size statistics")]
        titles =
          M.fromList
          [(russianResultLocale, "стат. длины очереди"),
           (englishResultLocale, "queue size stats")]

-- | Property 'facilityTotalWaitTime'.
facilityTotalWaitTimeId :: ResultId
facilityTotalWaitTimeId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "общее время ожидания прибора"),
           (englishResultLocale, "the total wait time")]
        titles =
          M.fromList
          [(russianResultLocale, "общее время ожидания"),
           (englishResultLocale, "total wait time")]

-- | Property 'facilityWaitTime'.
facilityWaitTimeId :: ResultId
facilityWaitTimeId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "время ожидания прибора"),
           (englishResultLocale, "the wait time")]
        titles =
          M.fromList
          [(russianResultLocale, "время ожидания"),
           (englishResultLocale, "wait time")]

-- | Property 'facilityTotalHoldingTime'.
facilityTotalHoldingTimeId :: ResultId
facilityTotalHoldingTimeId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "общее время удержания прибора"),
           (englishResultLocale, "the total holding time")]
        titles =
          M.fromList
          [(russianResultLocale, "общее время удержания"),
           (englishResultLocale, "total holding time")]

-- | Property 'facilityHoldingTime'.
facilityHoldingTimeId :: ResultId
facilityHoldingTimeId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "время удержания прибора"),
           (englishResultLocale, "the holding time")]
        titles =
          M.fromList
          [(russianResultLocale, "время удержания"),
           (englishResultLocale, "holding time")]

-- | Property 'facilityInterrupted'.
facilityInterruptedId :: ResultId
facilityInterruptedId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "удержание прибора сейчас прервано?"),
           (englishResultLocale, "is the facility interrupted now?")]
        titles =
          M.fromList
          [(russianResultLocale, "удержание прервано сейчас?"),
           (englishResultLocale, "interrupted now?")]

-- | The storage identifier.
storageId :: ResultId
storageId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "многоканальное устройство"),
           (englishResultLocale, "the storage")]
        titles =
          M.fromList
          [(russianResultLocale, "многоканальное устройство"),
           (englishResultLocale, "storage")]

-- | Property 'storageCapacity'.
storageCapacityId :: ResultId
storageCapacityId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "емкость многоканального устройства"),
           (englishResultLocale, "the storage capacity")]
        titles =
          M.fromList
          [(russianResultLocale, "емкость"),
           (englishResultLocale, "capacity")]

-- | Property 'storageEmpty'.
storageEmptyId :: ResultId
storageEmptyId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "не используется ли совсем устройство сейчас?"),
           (englishResultLocale, "is the storage unused completely now?")]
        titles =
          M.fromList
          [(russianResultLocale, "не используется сейчас?"),
           (englishResultLocale, "unused now?")]

-- | Property 'storageFull'.
storageFullId :: ResultId
storageFullId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "используется ли устройство полностью сейчас?"),
           (englishResultLocale, "is the storage used completely now?")]
        titles =
          M.fromList
          [(russianResultLocale, "используется полностью сейчас?"),
           (englishResultLocale, "used completely now?")]

-- | Property 'storageContent'.
storageContentId :: ResultId
storageContentId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "доступное содержимое устройства"),
           (englishResultLocale, "the current available content")]
        titles =
          M.fromList
          [(russianResultLocale, "доступное содержимое"),
           (englishResultLocale, "available content")]

-- | Property 'storageContentStats'.
storageContentStatsId :: ResultId
storageContentStatsId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "статистика доступного содержимого устройства"),
           (englishResultLocale, "the available content statistics")]
        titles =
          M.fromList
          [(russianResultLocale, "стат. доступного содержимого"),
           (englishResultLocale, "available content stats")]

-- | Property 'storageUseCount'.
storageUseCountId :: ResultId
storageUseCountId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "общее количество использований устройства"),
           (englishResultLocale, "the total use count")]
        titles =
          M.fromList
          [(russianResultLocale, "общее кол-во использований"),
           (englishResultLocale, "total use count")]

-- | Property 'storageUsedContent'.
storageUsedContentId :: ResultId
storageUsedContentId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "общее использованное количество устройства"),
           (englishResultLocale, "the total used content")]
        titles =
          M.fromList
          [(russianResultLocale, "общее использованное кол-во"),
           (englishResultLocale, "total used content")]

-- | Property 'storageUtilisationCount'.
storageUtilisationCountId :: ResultId
storageUtilisationCountId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "текущее используемое количество устройства"),
           (englishResultLocale, "the current utilisation count")]
        titles =
          M.fromList
          [(russianResultLocale, "используемое кол-во"),
           (englishResultLocale, "utilisation count")]

-- | Property 'storageUtilisationCountStats'.
storageUtilisationCountStatsId :: ResultId
storageUtilisationCountStatsId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "статистка по используемому количеству устройства"),
           (englishResultLocale, "the utilisation count statistics")]
        titles =
          M.fromList
          [(russianResultLocale, "стат. по используемому кол-ву"),
           (englishResultLocale, "utilisation count stats")]

-- | Property 'storageQueueCount'.
storageQueueCountId :: ResultId
storageQueueCountId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "текущая длина очереди к устройству"),
           (englishResultLocale, "the current queue size")]
        titles =
          M.fromList
          [(russianResultLocale, "длина очереди"),
           (englishResultLocale, "queue size")]

-- | Property 'storageQueueCountStats'.
storageQueueCountStatsId :: ResultId
storageQueueCountStatsId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "статистика длины очереди к устройству"),
           (englishResultLocale, "the queue size statistics")]
        titles =
          M.fromList
          [(russianResultLocale, "стат. длины очереди"),
           (englishResultLocale, "queue size stats")]

-- | Property 'storageTotalWaitTime'.
storageTotalWaitTimeId :: ResultId
storageTotalWaitTimeId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "общее время ожидания устройства"),
           (englishResultLocale, "the total wait time")]
        titles =
          M.fromList
          [(russianResultLocale, "общее время ожидания"),
           (englishResultLocale, "total wait time")]

-- | Property 'storageWaitTime'.
storageWaitTimeId :: ResultId
storageWaitTimeId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "время ожидания устройства"),
           (englishResultLocale, "the wait time")]
        titles =
          M.fromList
          [(russianResultLocale, "время ожидания"),
           (englishResultLocale, "wait time")]

-- | Property 'storageAverageHoldingTime'.
storageAverageHoldingTimeId :: ResultId
storageAverageHoldingTimeId =
  LocalisedResultId $
  LocalisedResult descrs titles
  where descrs =
          M.fromList
          [(russianResultLocale, "среднее время удержания устройства на единицу устройства"),
           (englishResultLocale, "the average holding time per unit")]
        titles =
          M.fromList
          [(russianResultLocale, "среднее время удержания"),
           (englishResultLocale, "average holding time")]
