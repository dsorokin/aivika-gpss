
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Test
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block TEST.
--
module Simulation.Aivika.GPSS.Block.Test
       (awaitingTestBlock,
        awaitingTestBlockM,
        transferringTestBlock,
        transferringTestBlockM) where

import Simulation.Aivika
import Simulation.Aivika.GPSS.Block

-- | This is the GPSS construct
--
-- @TEST O A,B@
awaitingTestBlock :: (a -> Signalable Bool)
                     -- ^ by the specified transact return
                     -- a test condition and signal that notifies
                     -- about changing the condition
                     -> Block a a
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
awaitingTestBlockM :: (a -> Process (Signalable Bool))
                      -- ^ by the specified transact return
                      -- a test condition and signal that notifies
                      -- about changing the condition
                      -> Block a a
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
transferringTestBlock :: (a -> Bool)
                         -- ^ the predicate
                         -> Block a ()
                         -- ^ the block to transfer in when the condition fails
                         -> Block a a
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
transferringTestBlockM :: (a -> Process Bool)
                          -- ^ the predicate
                          -> Block a ()
                          -- ^ the block to transfer in when the condition fails
                          -> Block a a
transferringTestBlockM pred block =
  Block { blockProcess = \a ->
           do f <- pred a
              if f
                then return a
                else transferProcess (blockProcess block a)
        }
