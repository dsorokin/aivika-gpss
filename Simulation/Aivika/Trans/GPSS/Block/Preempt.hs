
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Preempt
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block Preempt.
--
module Simulation.Aivika.Trans.GPSS.Block.Preempt
       (preemptBlock,
        PreemptBlockMode(..),
        defaultPreemptBlockMode,
        toFacilityPreemptMode) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Transact
import Simulation.Aivika.Trans.GPSS.Block
import Simulation.Aivika.Trans.GPSS.Facility

-- | Specifies the Preempt block mode.
data PreemptBlockMode m a =
  PreemptBlockMode { preemptBlockPriorityMode :: Bool,
                     -- ^ the Priority mode; otherwise, the Interrupt mode
                     preemptBlockTransfer :: Maybe (Maybe Double -> Block m (Transact m a) ()),
                     -- ^ where to transfer the preempted transact,
                     -- passing in the remaining time in the ADVANCE block
                     preemptBlockRemoveMode :: Bool
                     -- ^ the Remove mode
                   }

-- | Convert 'PreemptBlockMode' to 'FacilityPreemptMode'.
toFacilityPreemptMode :: MonadDES m => PreemptBlockMode m a -> Transact m a -> FacilityPreemptMode m
{-# INLINABLE toFacilityPreemptMode #-}
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
defaultPreemptBlockMode :: MonadDES m => PreemptBlockMode m a
{-# INLINABLE defaultPreemptBlockMode #-}
defaultPreemptBlockMode =
  PreemptBlockMode { preemptBlockPriorityMode = False,
                     preemptBlockTransfer     = Nothing,
                     preemptBlockRemoveMode   = False
                   }

-- | This is the GPSS construct
--
-- @PREEMPT A,B,C,D,E@
preemptBlock :: MonadDES m
                => Facility m a
                -- ^ the facility
                -> PreemptBlockMode m a
                -- ^ the Preempt block mode
                -> Block m (Transact m a) (Transact m a)
{-# INLINABLE preemptBlock #-}
preemptBlock r m =
  Block { blockProcess = \a -> preemptFacility r a (toFacilityPreemptMode m a) >> return a }
