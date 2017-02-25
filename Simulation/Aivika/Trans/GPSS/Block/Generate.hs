
-- |
-- Module     : Simulation.Aivika.Trans.GPSS.Block.Generate
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines the GPSS block GENERATE.
--
module Simulation.Aivika.Trans.GPSS.Block.Generate
       (streamGeneratorBlock0,
        streamGeneratorBlock,
        streamGeneratorBlockM) where

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
