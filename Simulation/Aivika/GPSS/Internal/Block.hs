
-- |
-- Module     : Simulation.Aivika.GPSS.Internal.Block
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines a GPSS block.
--
module Simulation.Aivika.GPSS.Internal.Block
       (Block(..)) where

import Control.Monad
import Control.Monad.Trans
import qualified Control.Category as C

import Simulation.Aivika

import Simulation.Aivika.GPSS.Transact

-- | Represents a GPSS block.
data Block a b =
  Block { blockProcess :: Transact a -> Process (Transact b),
          -- ^ Process the transact.
          blockHeadQueueCount :: Event Int,
          -- ^ Return the block head queue size.
          blockCanEnter :: Event Bool
          -- ^ Whether the transact can enter the block.
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
