
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Link
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines an analog of the GPSS block LINK.
--
module Simulation.Aivika.Trans.GPSS.Block.Link
       (linkBlock) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Block

-- | This is an analog of the GPSS construct
--
-- @LINK A,B,C@
linkBlock :: MonadDES m
             => (a -> Process m (Either (Block m a ()) Bool))
             -- ^ try to link the transact and return either the next block to transfer
             -- or a flag indicating whether the transact process should be passivated
             -- in case of successful linking, i.e. storing in the queue
             -> Block m a a
{-# INLINABLE linkBlock #-}
linkBlock f =
  Block { blockProcess = \a ->
           do x <- f a
              case x of
                Left transfer ->
                  transferProcess $
                  blockProcess transfer a
                Right False ->
                  return a
                Right True ->
                  do passivateProcess
                     return a
        }
