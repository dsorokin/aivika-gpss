
-- |
-- Module     : Simulation.Aivika.GPSS.MatchChain
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines a GPSS Match Chain.
--
module Simulation.Aivika.GPSS.MatchChain
       (MatchChain,
        newMatchChain,
        matchTransact,
        transactMatching,
        transactMatchingChanged,
        transactMatchingChangedByTransact_,
        transactMatchingChangedByAssemblySet_) where

import Data.IORef

import Control.Monad
import Control.Monad.Trans

import qualified Data.HashMap.Lazy as HM

import Simulation.Aivika
import Simulation.Aivika.GPSS.Transact
import Simulation.Aivika.GPSS.AssemblySet

-- | Represents a Match Chain.
data MatchChain =
  MatchChain { matchChainMap :: IORef (HM.HashMap AssemblySet ProcessId),
               matchChainSource :: SignalSource AssemblySet
             }

-- | Create a new Match Chain.
newMatchChain :: Simulation MatchChain
newMatchChain =
  do map <- liftIO $ newIORef HM.empty
     src <- newSignalSource
     return MatchChain { matchChainMap = map,
                         matchChainSource = src
                       }

-- | Match the transact.
matchTransact :: MatchChain -> Transact a -> Process ()
matchTransact chain t =
  do (map, set) <-
       liftEvent $
       do map <- liftIO $ readIORef (matchChainMap chain)
          set <- transactAssemblySet t
          return (map, set)
     case HM.lookup set map of
       Just pid ->
         liftEvent $
           do liftIO $ modifyIORef (matchChainMap chain) $
                HM.delete set
              yieldEvent $
                triggerSignal (matchChainSource chain) set
              reactivateProcess pid
       Nothing ->
         do liftEvent $
              do pid <- requireTransactProcessId t
                 liftIO $ modifyIORef (matchChainMap chain) $
                   HM.insert set pid
                 yieldEvent $
                   triggerSignal (matchChainSource chain) set
            passivateProcess

-- | Test whether there is a matching transact.
transactMatching :: MatchChain -> AssemblySet -> Event Bool
transactMatching chain set =
  do map <- liftIO $ readIORef (matchChainMap chain)
     return (HM.member set map)

-- | Signal each time the 'transactMatching' flag changes.
transactMatchingChangedByAssemblySet_ :: MatchChain -> AssemblySet -> Signal ()
transactMatchingChangedByAssemblySet_ chain set =
  mapSignal (const ()) $
  filterSignal (== set) $
  transactMatchingChanged chain

-- | Signal each time the 'transactMatching' flag changes.
transactMatchingChangedByTransact_ :: MatchChain -> Transact a -> Signal ()
transactMatchingChangedByTransact_ chain t =
  mapSignal (const ()) $
  filterSignalM pred $
  transactMatchingChanged chain
    where pred set =
            do set' <- transactAssemblySet t
               return (set == set')

-- | Signal each time the 'transactMatching' flag changes.
transactMatchingChanged :: MatchChain -> Signal AssemblySet
transactMatchingChanged chain =
  publishSignal (matchChainSource chain)
