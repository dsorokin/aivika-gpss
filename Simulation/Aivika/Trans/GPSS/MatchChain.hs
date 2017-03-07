
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
        transactMatching) where

import Control.Monad
import Control.Monad.Trans

import qualified Data.HashMap.Lazy as HM

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Transact
import Simulation.Aivika.Trans.GPSS.AssemblySet

-- | Represents a Match Chain.
data MatchChain m =
  MatchChain { matchChainMap :: Ref m (HM.HashMap (AssemblySet m) (ProcessId m)) }

-- | Create a new Match Chain.
newMatchChain :: MonadDES m => Simulation m (MatchChain m)
{-# INLINABLE newMatchChain #-}
newMatchChain =
  do map <- newRef HM.empty
     return MatchChain { matchChainMap = map }

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
              reactivateProcess pid
       Nothing ->
         do liftEvent $
              do pid <- requireTransactProcessId t
                 modifyRef (matchChainMap chain) $
                   HM.insert set pid
            passivateProcess

-- | Test whether there is a matching transact.
transactMatching :: MonadDES m => MatchChain m -> Transact m a -> Event m Bool
{-# INLINABLE transactMatching #-}
transactMatching chain t =
  do map <- readRef (matchChainMap chain)
     set <- transactAssemblySet t
     return (HM.member set map)
