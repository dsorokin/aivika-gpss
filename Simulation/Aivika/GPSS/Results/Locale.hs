
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
