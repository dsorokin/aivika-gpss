
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Enter
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block ENTER.
--
module Simulation.Aivika.GPSS.Block.Enter
       (enterBlock) where

import Simulation.Aivika
import Simulation.Aivika.GPSS.Transact
import Simulation.Aivika.GPSS.Block
import Simulation.Aivika.GPSS.Storage

-- | This is the GPSS construct
--
-- @ENTER A,B@
enterBlock :: Storage
              -- ^ the storage
              -> Int
              -- ^ the content decrement
              -> Block (Transact a) (Transact a)
enterBlock r decrement =
  Block { blockProcess = \a -> enterStorage r a decrement >> return a }
