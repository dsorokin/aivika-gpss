
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Gather
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block GATHER.
--
module Simulation.Aivika.Trans.GPSS.Block.Gather
       (gatherBlock) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Block
import Simulation.Aivika.Trans.GPSS.AssemblySet
import Simulation.Aivika.Trans.GPSS.Transact

-- | This is the GPSS construct
--
-- @GATHER A@
gatherBlock :: MonadDES m
               => Int
               -- ^ the number of transacts to gather
               -> Block m (Transact m a) (Transact m a)
{-# INLINABLE gatherBlock #-}
gatherBlock n =
  Block { blockProcess = \a -> gatherTransacts a n >> return a }
