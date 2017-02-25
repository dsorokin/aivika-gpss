
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines a GPSS block.
--
module Simulation.Aivika.Trans.GPSS.Block
       (Block(..),
        GeneratorBlock(..)) where

import Control.Monad
import Control.Monad.Trans
import qualified Control.Category as C

import Simulation.Aivika.Trans

-- | Represents a GPSS block.
data Block m a b =
  Block { blockProcess :: a -> Process m b,
          -- ^ Process the item.
          blockHeadQueueCount :: Event m Int,
          -- ^ Return the block head queue size.
          blockCanEnter :: Event m Bool
          -- ^ Whether an item can enter the block.
        }

-- | Represents a GPSS generator block.
newtype GeneratorBlock m a =
  GeneratorBlock { runGeneratorBlock :: Block m a () -> Process m () }

instance MonadDES m => C.Category (Block m) where

  {-# INLINABLE id #-}
  id =
    Block { blockProcess = return,
            blockHeadQueueCount = return 0,
            blockCanEnter = return True
          }

  {-# INLINABLE (.) #-}
  x . y =
    Block { blockProcess = \a -> do { b <- blockProcess y a; blockProcess x b },
            blockHeadQueueCount = blockHeadQueueCount y,
            blockCanEnter = blockCanEnter y
          }
