
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Test
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block TEST.
--
module Simulation.Aivika.Trans.GPSS.Block.Test
       (awaitingTestBlock,
        awaitingTestBlockM,
        transferringTestBlock,
        transferringTestBlockM) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Block

-- | This is the GPSS construct
--
-- @TEST O A,B@
awaitingTestBlock :: MonadDES m
                     => (a -> Signalable m Bool)
                     -- ^ by the specified transact return
                     -- a test condition and signal that notifies
                     -- about changing the condition
                     -> Block m a a
{-# INLINABLE awaitingTestBlock #-}
awaitingTestBlock f =
  Block { blockProcess = \a ->
           do let s = f a
                  loop =
                    do f <- liftEvent $ readSignalable s
                       if f
                         then return ()
                         else do processAwait $ signalableChanged_ s
                                 loop
              loop
              return a
        }

-- | This is the GPSS construct
--
-- @TEST O A,B@
awaitingTestBlockM :: MonadDES m
                      => (a -> Process m (Signalable m Bool))
                      -- ^ by the specified transact return
                      -- a test condition and signal that notifies
                      -- about changing the condition
                      -> Block m a a
{-# INLINABLE awaitingTestBlockM #-}
awaitingTestBlockM f =
  Block { blockProcess = \a ->
           do s <- f a
              let loop =
                    do f <- liftEvent $ readSignalable s
                       if f
                         then return ()
                         else do processAwait $ signalableChanged_ s
                                 loop
              loop
              return a
        }

-- | This is the GPSS construct
--
-- @TEST O A,B,C@
transferringTestBlock :: MonadDES m
                         => (a -> Bool)
                         -- ^ the predicate
                         -> Block m a ()
                         -- ^ the block to transfer in when the condition fails
                         -> Block m a a
{-# INLINABLE transferringTestBlock #-}
transferringTestBlock pred block =
  Block { blockProcess = \a ->
           do let f = pred a
              if f
                then return a
                else transferProcess (blockProcess block a)
        }

-- | This is the GPSS construct
--
-- @TEST O A,B,C@
transferringTestBlockM :: MonadDES m
                          => (a -> Process m Bool)
                          -- ^ the predicate
                          -> Block m a ()
                          -- ^ the block to transfer in when the condition fails
                          -> Block m a a
{-# INLINABLE transferringTestBlockM #-}
transferringTestBlockM pred block =
  Block { blockProcess = \a ->
           do f <- pred a
              if f
                then return a
                else transferProcess (blockProcess block a)
        }
