
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Advance
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block ADVANCE.
--
module Simulation.Aivika.Trans.GPSS.Block.Advance
       (advanceBlock) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Block

-- | This is the GPSS construct
--
-- @ADVANCE A,B@
advanceBlock :: MonadDES m
                => Process m ()
                -- ^ the delay
                -> Block m a a
{-# INLINABLE advanceBlock #-}
advanceBlock p =
  Block { blockProcess = \a -> p >> return a }
