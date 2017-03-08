
-- |
-- Module     : Simulation.Aivika.GPSS.MatchChain
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
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
        transactMatchingChanged_) where

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
transactMatching :: MatchChain -> Transact a -> Event Bool
transactMatching chain t =
  do map <- liftIO $ readIORef (matchChainMap chain)
     set <- transactAssemblySet t
     return (HM.member set map)

-- | Signal each time the 'transactMatching' flag changes.
transactMatchingChanged_ :: MatchChain -> Transact a -> Signal ()
transactMatchingChanged_ chain t =
  mapSignal (const ()) $
  filterSignalM pred $
  publishSignal (matchChainSource chain)
    where pred set =
            do set' <- transactAssemblySet t
               return (set == set')
