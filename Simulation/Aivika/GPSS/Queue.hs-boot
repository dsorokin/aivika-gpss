
-- |
-- Module     : Simulation.Aivika.GPSS.Queue
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This is an hs-boot file.
--
module Simulation.Aivika.GPSS.Queue
       (Queue,
        QueueEntry,
        entryQueue) where

import Data.Hashable

data Queue
data QueueEntry

instance Eq Queue
instance Hashable Queue

instance Eq QueueEntry

entryQueue :: QueueEntry -> Queue
