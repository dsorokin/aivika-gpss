
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
        runBlocks) where

import Control.Monad
import Control.Monad.Trans
import qualified Control.Category as C

import Simulation.Aivika

-- | Represents a GPSS block.
data Block a b =
  Block { blockProcess :: a -> Process b,
          -- ^ Process the item.
          blockHeadQueueCount :: Event Int,
          -- ^ Return the block head queue size.
          blockCanEnter :: Event Bool
          -- ^ Whether an item can enter the block.
        }

-- | Represents a GPSS generator block.
data GeneratorBlock a =
  GeneratorBlock { generatorItemDelay :: Process Double,
                   -- ^ A computation of the item delay.
                   generatorItemHandler :: Block a () -> Double -> Event ()
                   -- ^ Handle the item by the specified block and delay.
                 }

instance C.Category Block where

  id =
    Block { blockProcess = return,
            blockHeadQueueCount = return 0,
            blockCanEnter = return True
          }

  x . y =
    Block { blockProcess = \a -> do { b <- blockProcess y a; blockProcess x b },
            blockHeadQueueCount = blockHeadQueueCount y,
            blockCanEnter = blockCanEnter y
          }

-- | Run the GPSS blocks.
runBlocks :: GeneratorBlock a -> Block a () -> Process ()
runBlocks g x =
  let loop =
        do dt <- generatorItemDelay g
           liftEvent $
             generatorItemHandler g x dt
           loop
  in loop
