
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Assemble
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block ASSEMBLE.
--
module Simulation.Aivika.GPSS.Block.Assemble
       (assembleBlock) where

import Simulation.Aivika
import Simulation.Aivika.GPSS.Block
import Simulation.Aivika.GPSS.AssemblySet
import Simulation.Aivika.GPSS.Transact

-- | This is the GPSS construct
--
-- @ASSEMBLE A@
assembleBlock :: Int
                 -- ^ the number of transacts to assemble
                 -> Block (Transact a) (Transact a)
assembleBlock n =
  Block { blockProcess = \a -> assembleTransact a n >> return a }
