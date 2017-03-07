
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
        transactMatching) where

import Data.IORef

import Control.Monad
import Control.Monad.Trans

import qualified Data.HashMap.Lazy as HM

import Simulation.Aivika
import Simulation.Aivika.GPSS.Transact
import Simulation.Aivika.GPSS.AssemblySet

-- | Represents a Match Chain.
data MatchChain =
  MatchChain { matchChainMap :: IORef (HM.HashMap AssemblySet ProcessId) }

-- | Create a new Match Chain.
newMatchChain :: Simulation MatchChain
newMatchChain =
  do map <- liftIO $ newIORef HM.empty
     return MatchChain { matchChainMap = map }

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
              reactivateProcess pid
       Nothing ->
         do liftEvent $
              do pid <- requireTransactProcessId t
                 liftIO $ modifyIORef (matchChainMap chain) $
                   HM.insert set pid
            passivateProcess

-- | Test whether there is a matching transact.
transactMatching :: MatchChain -> Transact a -> Event Bool
transactMatching chain t =
  do map <- liftIO $ readIORef (matchChainMap chain)
     set <- transactAssemblySet t
     return (HM.member set map)
