
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Generate
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block GENERATE.
--
module Simulation.Aivika.Trans.GPSS.Block.Generate
       (streamGeneratorBlock0,
        streamGeneratorBlock,
        streamGeneratorBlockM,
        signalGeneratorBlock0,
        signalGeneratorBlock,
        signalGeneratorBlockM) where

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS.Block
import Simulation.Aivika.Trans.GPSS.Transact

-- | Return a generator block by the specified stream and priority computation.
streamGeneratorBlockM :: MonadDES m
                         => Stream m (Arrival a)
                         -- ^ the input stream of data
                         -> Event m Int
                         -- ^ the transact priority
                         -> GeneratorBlock m (Transact m a)
{-# INLINABLE streamGeneratorBlockM #-}
streamGeneratorBlockM s priority =
  let loop s block =
        do (a, xs) <- runStream s
           liftEvent $
             do p <- priority
                t <- liftSimulation $ newTransact a p
                runProcess $
                  do takeTransact t
                     blockProcess block t
           loop xs block
  in GeneratorBlock (loop s)

-- | Return a generator block by the specified stream and priority.
streamGeneratorBlock :: MonadDES m
                        => Stream m (Arrival a)
                        -- ^ the input stream of data
                        -> Int
                        -- ^ the transact priority
                        -> GeneratorBlock m (Transact m a)
{-# INLINABLE streamGeneratorBlock #-}
streamGeneratorBlock s = streamGeneratorBlockM s . return

-- | Return a generator block by the specified stream using zero priority.
streamGeneratorBlock0 :: MonadDES m
                         => Stream m (Arrival a)
                         -- ^ the input stream of data
                         -> GeneratorBlock m (Transact m a)
{-# INLINABLE streamGeneratorBlock0 #-}
streamGeneratorBlock0 s = streamGeneratorBlock s 0

-- | Return a generator block by the specified signal and priority computation.
signalGeneratorBlockM :: MonadDES m
                         => Signal m (Arrival a)
                         -- ^ the input signal of data
                         -> Event m Int
                         -- ^ the transact priority
                         -> GeneratorBlock m (Transact m a)
{-# INLINABLE signalGeneratorBlockM #-}
signalGeneratorBlockM s priority =
  let handle block a =
        do p <- priority
           t <- liftSimulation $ newTransact a p
           runProcess $
             do takeTransact t
                blockProcess block t
  in GeneratorBlock $ \block ->
  do h <- liftEvent $
          handleSignal s $
          handle block
     finallyProcess neverProcess
       (liftEvent $ disposeEvent h)

-- | Return a generator block by the specified signal and priority.
signalGeneratorBlock :: MonadDES m
                        => Signal m (Arrival a)
                        -- ^ the input signal of data
                        -> Int
                        -- ^ the transact priority
                        -> GeneratorBlock m (Transact m a)
{-# INLINABLE signalGeneratorBlock #-}
signalGeneratorBlock s = signalGeneratorBlockM s . return

-- | Return a generator block by the specified signal using zero priority.
signalGeneratorBlock0 :: MonadDES m
                         => Signal m (Arrival a)
                         -- ^ the input signal of data
                         -> GeneratorBlock m (Transact m a)
{-# INLINABLE signalGeneratorBlock0 #-}
signalGeneratorBlock0 s = signalGeneratorBlock s 0
