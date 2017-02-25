
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Internal.Block
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines a GPSS block.
--
module Simulation.Aivika.Trans.GPSS.Internal.Block
       (Block(..)) where

import Control.Monad
import Control.Monad.Trans
import qualified Control.Category as C

import Simulation.Aivika.Trans

import Simulation.Aivika.Trans.GPSS.Transact

-- | Represents a GPSS block.
data Block m a b =
  Block { blockProcess :: Transact m a -> Process m (Transact m b),
          -- ^ Process the transact.
          blockHeadQueueCount :: Event m Int
          -- ^ Return the block head queue size.
        }

instance MonadDES m => C.Category (Block m) where

  {-# INLINABLE id #-}
  id =
    Block { blockProcess = return,
            blockHeadQueueCount = return 0
          }

  {-# INLINABLE (.) #-}
  x . y =
    Block { blockProcess = \a -> do { b <- blockProcess y a; blockProcess x b },
            blockHeadQueueCount = blockHeadQueueCount x
          }
