
{-# LANGUAGE KindSignatures #-}

-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Queue
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This is an hs-boot file.
--
module Simulation.Aivika.Trans.GPSS.Queue
       (Queue,
        QueueEntry,
        entryQueue) where

import Data.Hashable

import Simulation.Aivika.Trans

data Queue m =
  Queue { queueSequenceNo :: Int,
          queueContentRef :: Ref m Int,
          queueContentStatsRef :: Ref m (TimingStats Int),
          enqueueCountRef :: Ref m Int,
          enqueueZeroEntryCountRef :: Ref m Int,
          queueWaitTimeRef :: Ref m (SamplingStats Double),
          queueNonZeroEntryWaitTimeRef :: Ref m (SamplingStats Double),
          enqueuedSource :: SignalSource m (),
          dequeuedSource :: SignalSource m ()
        }

data QueueEntry m =
  QueueEntry { entryQueue :: Queue m,
               entryEnqueueTime :: Double
             }

instance MonadDES m => Eq (Queue m)
instance Hashable (Queue m)

instance MonadDES m => Eq (QueueEntry m)
