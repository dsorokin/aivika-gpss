
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Match
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block MATCH.
--
module Simulation.Aivika.Trans.GPSS.Block.Match
       (matchBlock) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Block
import Simulation.Aivika.Trans.GPSS.MatchChain
import Simulation.Aivika.Trans.GPSS.Transact

-- | This is an analog of the GPSS construct
--
-- @MATCH A@
matchBlock :: MonadDES m
              => MatchChain m
              -- ^ the corresponding match chain
              -> Block m (Transact m a) (Transact m a)
{-# INLINABLE matchBlock #-}
matchBlock chain =
  Block { blockProcess = \a -> matchTransact chain a >> return a }
