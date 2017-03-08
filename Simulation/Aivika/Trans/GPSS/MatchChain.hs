
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.MatchChain
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
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
        transactMatchingChanged_) where

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
transactMatching :: MonadDES m => MatchChain m -> Transact m a -> Event m Bool
{-# INLINABLE transactMatching #-}
transactMatching chain t =
  do map <- readRef (matchChainMap chain)
     set <- transactAssemblySet t
     return (HM.member set map)

-- | Signal each time the 'transactMatching' flag changes.
transactMatchingChanged_ :: MonadDES m => MatchChain m -> Transact m a -> Signal m ()
{-# INLINABLE transactMatchingChanged_ #-}
transactMatchingChanged_ chain t =
  mapSignal (const ()) $
  filterSignalM pred $
  publishSignal (matchChainSource chain)
    where pred set =
            do set' <- transactAssemblySet t
               return (set == set')
