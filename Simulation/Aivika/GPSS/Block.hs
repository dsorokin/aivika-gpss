
-- |
-- Module     : Simulation.Aivika.GPSS.Block
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines a GPSS block.
--
module Simulation.Aivika.GPSS.Block
       (Block(..),
        GeneratorBlock(..),
        withinBlock,
        traceBlock) where

import Control.Monad
import Control.Monad.Trans
import qualified Control.Category as C

import Simulation.Aivika

-- | Represents a GPSS block.
data Block a b =
  Block { blockProcess :: a -> Process b
          -- ^ Process the item.
        }

-- | Represents a GPSS generator block.
newtype GeneratorBlock a =
  GeneratorBlock { runGeneratorBlock :: Block a () -> Process ()
                   -- ^ Run the generator block.
                 }

instance C.Category Block where

  id = Block { blockProcess = return }

  x . y = Block { blockProcess = \a -> do { b <- blockProcess y a; blockProcess x b } }

-- | Perform some action within the block, for example,
-- opening or inverting the 'Gate' to emulate the LOGIC block.
withinBlock :: Process ()
               -- ^ the action to be executed for each transact
               -> Block a a
withinBlock m =
  Block { blockProcess = \a -> m >> return a }

-- | Trace the specified block.
traceBlock :: String -> Block a b -> Block a b
traceBlock message x =
  Block { blockProcess = \a -> traceProcess message (blockProcess x a) }
