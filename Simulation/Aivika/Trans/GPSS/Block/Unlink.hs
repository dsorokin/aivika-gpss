
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Unlink
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines an analog of the GPSS block UNLINK.
--
module Simulation.Aivika.Trans.GPSS.Block.Unlink
       (unlinkBlock) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Block
import Simulation.Aivika.Trans.GPSS.Transact

-- | This is an analog of the GPSS construct
--
-- @UNLINK O A,B,C,D,E,F@
unlinkBlock :: MonadDES m
               => Process m [(Transact m a, Maybe (Block m (Transact m a) ()))]
               -- ^ a computation of the list of transacts to reactivate,
               -- transfering them to the specified blocks if required
               -> Block m b b
{-# INLINABLE unlinkBlock #-}
unlinkBlock m =
  Block { blockProcess = \b ->
           do let f (a, Nothing)       = (a, Nothing)
                  f (a, Just transfer) = (a, Just $ blockProcess transfer a)
              xs <- m
              liftEvent $
                reactivateTransacts $
                map f xs
              return b
        }
