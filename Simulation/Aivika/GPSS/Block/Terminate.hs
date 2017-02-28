
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Terminate
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block TERMINATE.
--
module Simulation.Aivika.GPSS.Block.Terminate
       (terminateBlock,
        terminateBlockByCount,
        terminateBlockByCountM) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika
import Simulation.Aivika.GPSS.Block

-- | This is the GPSS construct
--
-- @TERMINATE@
terminateBlock :: Block a ()
terminateBlock =
  Block { blockProcess = \a -> return () }

-- | This is the GPSS construct
--
-- @TERMINATE Count@
terminateBlockByCountM :: Ref Int
                          -- ^ the counter
                          -> Event Int
                          -- ^ the computation of decrement
                          -> Block a ()
terminateBlockByCountM counter decrement =
  Block { blockProcess = \a -> action }
    where
      action = 
        liftEvent $
        do i <- decrement
           n <- readRef counter
           let n' = n - i
           n' `seq` writeRef counter n'
           when (n' <= 0) $
             throwEvent $
             SimulationAbort "Terminated by exceeding the counter"

-- | This is the GPSS construct
--
-- @TERMINATE Count@
terminateBlockByCount :: Ref Int
                         -- ^ the counter
                         -> Int
                         -- ^ the decrement
                         -> Block a ()
terminateBlockByCount counter i =
  Block { blockProcess = \a -> action }
    where
      action = 
        liftEvent $
        do n <- readRef counter
           let n' = n - i
           n' `seq` writeRef counter n'
           when (n' <= 0) $
             throwEvent $
             SimulationAbort "Terminated by exceeding the counter"
