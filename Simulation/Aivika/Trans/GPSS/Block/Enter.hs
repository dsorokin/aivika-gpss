
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Enter
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block ENTER.
--
module Simulation.Aivika.Trans.GPSS.Block.Enter
       (enterBlock) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Transact
import Simulation.Aivika.Trans.GPSS.Block
import Simulation.Aivika.Trans.GPSS.Storage

-- | This is the GPSS construct
--
-- @ENTER A,B@
enterBlock :: MonadDES m
              => Storage m
              -- ^ the storage
              -> Int
              -- ^ the content decrement
              -> Block m (Transact m a) (Transact m a)
{-# INLINABLE enterBlock #-}
enterBlock r decrement =
  Block { blockProcess = \a -> enterStorage r a decrement >> return a }
