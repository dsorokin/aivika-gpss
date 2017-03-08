
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Leave
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block LEAVE.
--
module Simulation.Aivika.GPSS.Block.Leave
       (leaveBlock) where

import Simulation.Aivika
import Simulation.Aivika.GPSS.Transact
import Simulation.Aivika.GPSS.Block
import Simulation.Aivika.GPSS.Storage

-- | This is an analog of the GPSS construct
--
-- @LEAVE A,B@
--
-- Note that unlike GPSS the leaving of Storage has immediate effect, which means
-- that another transact may enter the Storage before the current transact
-- finishes leaving the block.
leaveBlock :: Storage
              -- ^ the storage
              -> Int
              -- ^ the content increment
              -> Block (Transact a) (Transact a)
leaveBlock r increment =
  Block { blockProcess = \a -> leaveStorage r increment >> return a }
