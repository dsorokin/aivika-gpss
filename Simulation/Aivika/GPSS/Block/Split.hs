
-- |
-- Module     : Simulation.Aivika.GPSS.Block.Split
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module defines an analog of the GPSS block SPLIT.
--
module Simulation.Aivika.GPSS.Block.Split
       (splitBlock) where

import Simulation.Aivika
import Simulation.Aivika.GPSS.Block
import Simulation.Aivika.GPSS.Transact

-- | This is an analog of the GPSS construct
--
-- @SPLIT A,B,C@
--
-- Parameter @A@ is a length of the list parameter passed in to the function.
-- Parameter @B@ is the list itself. If you need to define parameter @C@ then
-- you can create the blocks dynamically that could depend on the index and
-- where we could assign a new value for each new transcact after splitting.
--
-- An example is
--
-- @
-- let blocks :: [Block (Transact (a, Int)) ()]
--     blocks = ...
--     f :: (Int, Block (Transact (a, Int)) ()) -> Block (Transact a) ()
--     f (n, block) = assignBlock (\a -> (a, n)) >>> block
--     blocks' :: [Block (Transact a) ()]
--     blocks' = map f $ zip [0..] blocks
-- in splitBlock blocks'
-- @
splitBlock :: [Block (Transact a) ()]
              -- ^ split and transfer new transacts to the specified blocks
              -> Block (Transact a) (Transact a)
splitBlock blocks =
  Block { blockProcess = \a ->
           do let loop [] = return ()
                  loop (transfer: transfers) =
                    do a' <- liftSimulation $ splitTransact a
                       transferTransact a' $
                         blockProcess transfer a'
                       loop transfers
              liftEvent $ loop blocks
              return a
        }
