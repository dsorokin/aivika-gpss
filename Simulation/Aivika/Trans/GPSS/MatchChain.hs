
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.MatchChain
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines a GPSS Match Chain.
--
module Simulation.Aivika.Trans.GPSS.MatchChain
       (MatchChain,
        newMatchChain,
        matchTransact,
        transactMatching,
        transactMatchingChanged,
        transactMatchingChangedByTransact_,
        transactMatchingChangedByAssemblySet_) where

import Control.Monad
import Control.Monad.Trans

import qualified Data.HashMap.Lazy as HM

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Transact
import Simulation.Aivika.Trans.GPSS.AssemblySet

-- | Represents a Match Chain.
data MatchChain m =
  MatchChain { matchChainMap :: Ref m (HM.HashMap (AssemblySet m) (ProcessId m)),
               matchChainSource :: SignalSource m (AssemblySet m)
             }

-- | Create a new Match Chain.
newMatchChain :: MonadDES m => Simulation m (MatchChain m)
{-# INLINABLE newMatchChain #-}
newMatchChain =
  do map <- newRef HM.empty
     src <- newSignalSource
     return MatchChain { matchChainMap = map,
                         matchChainSource = src
                       }

-- | Match the transact.
matchTransact :: MonadDES m => MatchChain m -> Transact m a -> Process m ()
{-# INLINABLE matchTransact #-}
matchTransact chain t =
  do (map, set) <-
       liftEvent $
       do map <- readRef (matchChainMap chain)
          set <- transactAssemblySet t
          return (map, set)
     case HM.lookup set map of
       Just pid ->
         liftEvent $
           do modifyRef (matchChainMap chain) $
                HM.delete set
              yieldEvent $
                triggerSignal (matchChainSource chain) set
              reactivateProcess pid
       Nothing ->
         do liftEvent $
              do pid <- requireTransactProcessId t
                 modifyRef (matchChainMap chain) $
                   HM.insert set pid
                 yieldEvent $
                   triggerSignal (matchChainSource chain) set
            passivateProcess

-- | Test whether there is a matching transact.
transactMatching :: MonadDES m => MatchChain m -> AssemblySet m -> Event m Bool
{-# INLINABLE transactMatching #-}
transactMatching chain set =
  do map <- readRef (matchChainMap chain)
     return (HM.member set map)

-- | Signal each time the 'transactMatching' flag changes.
transactMatchingChangedByAssemblySet_ :: MonadDES m => MatchChain m -> AssemblySet m -> Signal m ()
{-# INLINABLE transactMatchingChangedByAssemblySet_ #-}
transactMatchingChangedByAssemblySet_ chain set =
  mapSignal (const ()) $
  filterSignal (== set) $
  transactMatchingChanged chain

-- | Signal each time the 'transactMatching' flag changes.
transactMatchingChangedByTransact_ :: MonadDES m => MatchChain m -> Transact m a -> Signal m ()
{-# INLINABLE transactMatchingChangedByTransact_ #-}
transactMatchingChangedByTransact_ chain t =
  mapSignal (const ()) $
  filterSignalM pred $
  transactMatchingChanged chain
    where pred set =
            do set' <- transactAssemblySet t
               return (set == set')

-- | Signal each time the 'transactMatching' flag changes.
transactMatchingChanged :: MonadDES m => MatchChain m -> Signal m (AssemblySet m)
{-# INLINABLE transactMatchingChanged #-}
transactMatchingChanged chain =
  publishSignal (matchChainSource chain)
