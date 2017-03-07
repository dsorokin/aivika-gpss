
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Assemble
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block ASSEMBLE.
--
module Simulation.Aivika.Trans.GPSS.Block.Assemble
       (assembleBlock) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Block
import Simulation.Aivika.Trans.GPSS.AssemblySet
import Simulation.Aivika.Trans.GPSS.Transact

-- | This is the GPSS construct
--
-- @ASSEMBLE A@
assembleBlock :: MonadDES m
                 => Int
                 -- ^ the number of transacts to assemble
                 -> Block m (Transact m a) (Transact m a)
{-# INLINABLE assembleBlock #-}
assembleBlock n =
  Block { blockProcess = \a -> assembleTransact a n >> return a }
