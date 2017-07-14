
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.AssemblySet
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This is an hs-boot file.
--
module Simulation.Aivika.Trans.GPSS.AssemblySet
       (AssemblySet,
        newAssemblySet) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.TransactQueueStrategy

data AssemblySet m =
  AssemblySet { assemblySetSequenceNo :: Int,
                assemblySetAssemblingTransact :: Ref m (Maybe (ProcessId m)),
                assemblySetAssemblingCounter :: Ref m Int,
                assemblySetGatheringTransacts :: StrategyQueue m (TransactQueueStrategy FCFS) (ProcessId m),
                assemblySetGatheringCounter :: Ref m Int
              }

newAssemblySet :: MonadDES m => Simulation m (AssemblySet m)
