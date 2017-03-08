
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Gather
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block GATHER.
--
module Simulation.Aivika.GPSS.Block.Gather
       (gatherBlock) where

import Simulation.Aivika
import Simulation.Aivika.GPSS.Block
import Simulation.Aivika.GPSS.AssemblySet
import Simulation.Aivika.GPSS.Transact

-- | This is an analog of the GPSS construct
--
-- @GATHER A@
--
-- Note that unlike GPSS the transacts are reactivated in unspecified order.
gatherBlock :: Int
               -- ^ the number of transacts to gather
               -> Block (Transact a) (Transact a)
gatherBlock n =
  Block { blockProcess = \a -> gatherTransacts a n >> return a }
