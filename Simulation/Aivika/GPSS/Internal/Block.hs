
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
       (Block(..),
        BlockChain(..),
        blockProcessor,
        processorBlock) where

import Control.Monad
import Control.Monad.Trans
import qualified Control.Category as C

import Simulation.Aivika
import Simulation.Aivika.Queue.Infinite.Base

import Simulation.Aivika.GPSS.Transact

-- | Represents a GPSS block.
newtype Block a b =
  Block { unBlock :: FCFSQueue (Transact a) -> FCFSQueue (Transact b) -> BlockChain
                     -- ^ Unwrap the block.
        }

-- | Represents the block chain.
data BlockChain =
  BlockChain { blockChainProcess :: Process ()
               -- ^ The block chain process
             }
  
instance C.Category Block where

  id =
    Block $ \qi qo ->
    let action =
          do a <- dequeue qi
             liftEvent $ enqueue qo a
             action
    in BlockChain { blockChainProcess = action }

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
blockProcessor :: Block a b -> Processor (Transact a) (Transact b)
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
processorBlock :: Processor (Transact a) (Transact b) -> Block a b
processorBlock p =
  Block $ \qi qo ->
  let action =
        do let xs = repeatProcess (dequeue qi)
               ys = runProcessor p xs
           consumeStream (liftEvent . enqueue qo) ys
  in BlockChain { blockChainProcess = action }
