
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Preempt
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block Preempt.
--
module Simulation.Aivika.GPSS.Block.Preempt
       (preemptBlock,
        PreemptBlockMode(..),
        defaultPreemptBlockMode,
        toFacilityPreemptMode) where

import Simulation.Aivika
import Simulation.Aivika.GPSS.Transact
import Simulation.Aivika.GPSS.Block
import Simulation.Aivika.GPSS.Facility

-- | Specifies the Preempt block mode.
data PreemptBlockMode a =
  PreemptBlockMode { preemptBlockPriorityMode :: Bool,
                     -- ^ the Priority mode; otherwise, the Interrupt mode
                     preemptBlockTransfer :: Maybe (Maybe Double -> Block (Transact a) ()),
                     -- ^ where to transfer the preempted transact,
                     -- passing in the remaining time in the ADVANCE block
                     preemptBlockRemoveMode :: Bool
                     -- ^ the Remove mode
                   }

-- | Convert 'PreemptBlockMode' to 'FacilityPreemptMode'.
toFacilityPreemptMode :: PreemptBlockMode a -> Transact a -> FacilityPreemptMode
toFacilityPreemptMode m a =
  FacilityPreemptMode { facilityPriorityMode = preemptBlockPriorityMode m,
                        facilityTransfer     = transfer,
                        facilityRemoveMode   = preemptBlockRemoveMode m
                      }
  where
    transfer =
      case preemptBlockTransfer m of
        Nothing -> Nothing
        Just f  -> Just (\dt -> blockProcess (f dt) a)

-- | The default Preempt block mode.
defaultPreemptBlockMode :: PreemptBlockMode a
defaultPreemptBlockMode =
  PreemptBlockMode { preemptBlockPriorityMode = False,
                     preemptBlockTransfer     = Nothing,
                     preemptBlockRemoveMode   = False
                   }

-- | This is the GPSS construct
--
-- @PREEMPT A,B,C,D,E@
preemptBlock :: Facility a
                -- ^ the facility
                -> PreemptBlockMode a
                -- ^ the Preempt block mode
                -> Block (Transact a) (Transact a)
preemptBlock r m =
  Block { blockProcess = \a -> preemptFacility r a (toFacilityPreemptMode m a) >> return a }
