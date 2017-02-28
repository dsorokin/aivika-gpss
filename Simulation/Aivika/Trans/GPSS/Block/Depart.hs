
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Depart
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block DEPART.
--
module Simulation.Aivika.Trans.GPSS.Block.Depart
       (departBlock) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Transact
import Simulation.Aivika.Trans.GPSS.Block
import qualified Simulation.Aivika.Trans.GPSS.Queue as Q

-- | This is the GPSS construct
--
-- @DEPART A,B@
departBlock :: MonadDES m
               => Q.Queue m
               -- ^ the queue
               -> Int
               -- ^ the content decrement
               -> Block m (Transact m a) (Transact m a)
{-# INLINABLE departBlock #-}
departBlock q decrement =
  Block { blockProcess = \a -> (liftEvent $ Q.dequeue q a decrement) >> return a }
