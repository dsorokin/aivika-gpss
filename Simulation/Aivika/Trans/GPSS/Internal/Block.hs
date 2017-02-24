
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
       (Block(..),
        BlockChain(..),
        blockProcessor,
        processorBlock) where

import Control.Monad
import Control.Monad.Trans
import qualified Control.Category as C

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Queue.Infinite.Base

import Simulation.Aivika.Trans.GPSS.Transact

-- | Represents a GPSS block.
newtype Block m a b =
  Block { unBlock :: FCFSQueue m (Transact m a) -> FCFSQueue m (Transact m b) -> BlockChain m
                     -- ^ Unwrap the block.
        }

-- | Represents the block chain.
data BlockChain m =
  BlockChain { blockChainProcess :: Process m ()
               -- ^ The block chain process
             }
  
instance MonadDES m => C.Category (Block m) where

  {-# INLINABLE id #-}
  id =
    Block $ \qi qo ->
    let action =
          do a <- dequeue qi
             liftEvent $ enqueue qo a
             action
    in BlockChain { blockChainProcess = action }

  {-# INLINABLE (.) #-}
  x . y =
    Block $ \qi qo ->
    let action =
          do qm <- liftSimulation newFCFSQueue
             let ch1 = unBlock y qi qm
                 ch2 = unBlock x qm qo
             spawnProcess $ blockChainProcess ch1
             spawnProcess $ blockChainProcess ch2
    in BlockChain { blockChainProcess = action }

-- | Return the block processor.
blockProcessor :: MonadDES m => Block m a b -> Processor m (Transact m a) (Transact m b)
{-# INLINABLE blockProcessor #-}
blockProcessor b =
  Processor $ \xs ->
  Cons $
  do qi <- liftSimulation newFCFSQueue
     qo <- liftSimulation newFCFSQueue
     let ch = unBlock b qi qo
     spawnProcess $ blockChainProcess ch
     spawnProcess $ consumeStream (liftEvent . enqueue qi) xs
     runStream $ repeatProcess (dequeue qo)

-- | Return the processor block.
processorBlock :: MonadDES m => Processor m (Transact m a) (Transact m b) -> Block m a b
{-# INLINABLE processorBlock #-}
processorBlock p =
  Block $ \qi qo ->
  let action =
        do let deq =
                 do a <- dequeue qi
                    takeTransact a
                    return a
               enq b =
                 do releaseTransact b
                    liftEvent $ enqueue qo b
               xs = repeatProcess deq
               ys = runProcessor p xs
           consumeStream enq ys
  in BlockChain { blockChainProcess = action }
