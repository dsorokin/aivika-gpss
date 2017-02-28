
-- |
-- Module     : Simulation.Aivika.GPSS.Results.Locale
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
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

-- | Property 'Q.queueCount'.
queueCountId :: ResultId
queueCountId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "текущий размер очереди"),
   (englishResultLocale, "the current queue size")]

-- | Property 'Q.queueCountStats'.
queueCountStatsId :: ResultId
queueCountStatsId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "статистика по размеру очереди"),
   (englishResultLocale, "the queue size statistics")]

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

-- | Property 'Q.dequeueCount'.
dequeueCountId :: ResultId
dequeueCountId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "количество извлеченных транзактов"),
   (englishResultLocale, "the number of dequeued transacts")]

-- | Property 'Q.enqueueRate'.
enqueueRateId :: ResultId
enqueueRateId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "количество добавлений на ед. времени"),
   (englishResultLocale, "how many enqueued transacts per time?")]

-- | Property 'Q.dequeueRate'.
dequeueRateId :: ResultId
dequeueRateId =
  LocalisedResultId $
  M.fromList
  [(russianResultLocale, "количество извлечений на ед. времени"),
   (englishResultLocale, "how many dequeued transacts per time?")]

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
