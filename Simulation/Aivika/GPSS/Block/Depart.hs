
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Depart
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block DEPART.
--
module Simulation.Aivika.GPSS.Block.Depart
       (departBlock) where

import Simulation.Aivika
import Simulation.Aivika.GPSS.Transact
import Simulation.Aivika.GPSS.Block
import qualified Simulation.Aivika.GPSS.Queue as Q

-- | This is the GPSS construct
--
-- @DEPART A,B@
departBlock :: Q.Queue
               -- ^ the queue
               -> Int
               -- ^ the content decrement
               -> Block (Transact a) (Transact a)
departBlock q decrement =
  Block { blockProcess = \a -> (liftEvent $ Q.dequeue q a decrement) >> return a }
