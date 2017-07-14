
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Loop
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block LOOP.
--
module Simulation.Aivika.Trans.GPSS.Block.Loop
       (loopBlock,
        loopBlockM) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Block

-- | This is the GPSS construct
--
-- @LOOP A,B@
loopBlock :: MonadDES m
             => (a -> (b, Bool))
             -- ^ by the specified transact return the next version
             -- of the same transact and a condition whether we should
             -- exit the loop
             -> Block m b ()
             -- ^ the block to transfer in when the condition fails
             -> Block m a b
{-# INLINABLE loopBlock #-}
loopBlock f block =
  Block { blockProcess = \a ->
           do let (b, c) = f a
              if c
                then return b
                else transferProcess (blockProcess block b)
        }

-- | This is the GPSS construct
--
-- @LOOP A,B@
loopBlockM :: MonadDES m
              => (a -> Process m (b, Bool))
              -- ^ by the specified transact return the next version
              -- of the same transact and a condition whether we should
              -- exit the loop
              -> Block m b ()
              -- ^ the block to transfer in when the condition fails
              -> Block m a b
{-# INLINABLE loopBlockM #-}
loopBlockM f block =
  Block { blockProcess = \a ->
           do (b, c) <- f a
              if c
                then return b
                else transferProcess (blockProcess block b)
        }
