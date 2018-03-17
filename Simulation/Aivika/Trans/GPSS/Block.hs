
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines a GPSS block.
--
module Simulation.Aivika.Trans.GPSS.Block
       (Block(..),
        GeneratorBlock(..),
        withinBlock,
        processBlock,
        traceBlock) where

import Control.Monad
import Control.Monad.Trans
import qualified Control.Category as C

import Simulation.Aivika.Trans

-- | Represents a GPSS block.
newtype Block m a b =
  Block { blockProcess :: a -> Process m b
          -- ^ Process the item.
        }

-- | Represents a GPSS generator block.
newtype GeneratorBlock m a =
  GeneratorBlock { runGeneratorBlock :: Block m a () -> Process m ()
                   -- ^ Run the generator block.
                 }

instance MonadDES m => C.Category (Block m) where

  {-# INLINABLE id #-}
  id = Block { blockProcess = return }

  {-# INLINABLE (.) #-}
  x . y = Block { blockProcess = \a -> do { b <- blockProcess y a; blockProcess x b } }

-- | Perform some action within the block, for example,
-- opening or inverting the 'Gate' to emulate the LOGIC block.
withinBlock :: MonadDES m
               => Process m ()
               -- ^ the action to be executed for each transact
               -> Block m a a
{-# INLINABLE withinBlock #-}
withinBlock m =
  Block { blockProcess = \a -> m >> return a }

-- | Process every transact within the block.
processBlock :: MonadDES m
                => (a -> Process m b)
                -- ^ process the transact
                -> Block m a b
{-# INLINABLE processBlock #-}
processBlock = Block

-- | Trace the specified block.
traceBlock :: MonadDES m => String -> Block m a b -> Block m a b
{-# INLINABLE traceBlock #-}
traceBlock message x =
  Block { blockProcess = \a -> traceProcess message (blockProcess x a) }
