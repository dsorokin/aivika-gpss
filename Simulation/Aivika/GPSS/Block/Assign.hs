
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Assign
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block ASSIGN.
--
module Simulation.Aivika.GPSS.Block.Assign
       (assignBlock,
        assignBlockM) where

import Simulation.Aivika
import Simulation.Aivika.GPSS.Block
import Simulation.Aivika.GPSS.Transact

-- | This is the GPSS construct
--
-- @ASSIGN A,B,C@
assignBlock :: (a -> b)
               -- ^ the transform
               -> Block (Transact a) (Transact b)
assignBlock f =
  Block { blockProcess = \a -> return (assignTransactValue a f) }

-- | This is the GPSS construct
--
-- @ASSIGN A,B,C@
assignBlockM :: (a -> Process b)
                -- ^ the transform computation
                -> Block (Transact a) (Transact b)
assignBlockM f =
  Block { blockProcess = \a -> assignTransactValueM a f }
