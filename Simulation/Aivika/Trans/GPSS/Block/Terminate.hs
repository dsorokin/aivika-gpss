
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Terminate
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block TERMINATE.
--
module Simulation.Aivika.Trans.GPSS.Block.Terminate
       (terminateBlock,
        terminateBlockByCount,
        terminateBlockByCountM) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Block

-- | This is the GPSS construct
--
-- @TERMINATE@
terminateBlock :: MonadDES m => Block m a ()
{-# INLINABLE terminateBlock #-}
terminateBlock =
  Block { blockProcess = \a -> return () }

-- | This is the GPSS construct
--
-- @TERMINATE Count@
terminateBlockByCountM :: MonadDES m
                          => Ref m Int
                          -- ^ the counter
                          -> Event m Int
                          -- ^ the computation of decrement
                          -> Block m a ()
{-# INLINABLE terminateBlockByCountM #-}
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
terminateBlockByCount :: MonadDES m
                         => Ref m Int
                         -- ^ the counter
                         -> Int
                         -- ^ the decrement
                         -> Block m a ()
{-# INLINABLE terminateBlockByCount #-}
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
