
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Generate
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block GENERATE.
--
module Simulation.Aivika.GPSS.Block.Generate
       (streamGeneratorBlock0,
        streamGeneratorBlock,
        streamGeneratorBlockM,
        signalGeneratorBlock0,
        signalGeneratorBlock,
        signalGeneratorBlockM) where

import Simulation.Aivika
import Simulation.Aivika.GPSS.Block
import Simulation.Aivika.GPSS.Transact

-- | Return a generator block by the specified stream and priority computation.
streamGeneratorBlockM :: Stream (Arrival a)
                         -- ^ the input stream of data
                         -> Event Int
                         -- ^ the transact priority
                         -> GeneratorBlock (Transact a)
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
streamGeneratorBlock :: Stream (Arrival a)
                        -- ^ the input stream of data
                        -> Int
                        -- ^ the transact priority
                        -> GeneratorBlock (Transact a)
streamGeneratorBlock s = streamGeneratorBlockM s . return

-- | Return a generator block by the specified stream using zero priority.
streamGeneratorBlock0 :: Stream (Arrival a)
                         -- ^ the input stream of data
                         -> GeneratorBlock (Transact a)
streamGeneratorBlock0 s = streamGeneratorBlock s 0

-- | Return a generator block by the specified signal and priority computation.
signalGeneratorBlockM :: Signal (Arrival a)
                         -- ^ the input signal of data
                         -> Event Int
                         -- ^ the transact priority
                         -> GeneratorBlock (Transact a)
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
signalGeneratorBlock :: Signal (Arrival a)
                        -- ^ the input signal of data
                        -> Int
                        -- ^ the transact priority
                        -> GeneratorBlock (Transact a)
signalGeneratorBlock s = signalGeneratorBlockM s . return

-- | Return a generator block by the specified signal using zero priority.
signalGeneratorBlock0 :: Signal (Arrival a)
                         -- ^ the input signal of data
                         -> GeneratorBlock (Transact a)
signalGeneratorBlock0 s = signalGeneratorBlock s 0
