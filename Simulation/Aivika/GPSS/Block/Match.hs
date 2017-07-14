
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Match
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block MATCH.
--
module Simulation.Aivika.GPSS.Block.Match
       (matchBlock) where

import Simulation.Aivika
import Simulation.Aivika.GPSS.Block
import Simulation.Aivika.GPSS.MatchChain
import Simulation.Aivika.GPSS.Transact

-- | This is an analog of the GPSS construct
--
-- @MATCH A@
matchBlock :: MatchChain
              -- ^ the corresponding match chain
              -> Block (Transact a) (Transact a)
matchBlock chain =
  Block { blockProcess = \a -> matchTransact chain a >> return a }
