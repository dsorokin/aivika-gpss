
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Queue
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block QUEUE.
--
module Simulation.Aivika.GPSS.Block.Queue
       (queueBlock) where

import Simulation.Aivika
import Simulation.Aivika.GPSS.Transact
import Simulation.Aivika.GPSS.Block
import qualified Simulation.Aivika.GPSS.Queue as Q

-- | This is the GPSS construct
--
-- @QUEUE WaitingLine@
queueBlock :: Q.Queue
              -- ^ the queue
              -> Block (Transact a) (Transact a)
queueBlock q =
  Block { blockProcess = \a -> (liftEvent $ Q.enqueue q a) >> return a }
