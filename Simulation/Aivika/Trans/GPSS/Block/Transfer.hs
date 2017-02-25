
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Transfer
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block TRANSFER.
--
module Simulation.Aivika.Trans.GPSS.Block.Transfer
       (transferBlock) where

import Simulation.Aivika.Trans.GPSS.Internal.Block

-- | This is the GPSS construct
--
-- @TRANSFER ,New_Place@
transferBlock :: Block m a b -> Block m a b
{-# INLINABLE transferBlock #-}
transferBlock = id
