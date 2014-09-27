{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
--
-- Module      :  Cluster
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Cluster (
      Cluster
    , append
    , connect
    , nodesNum
    , adjList
    , sockets
    , second
    , single
    , square
) where

import Data.Map (Map, (!))
import Data.Array.ST (writeArray)
import Control.Monad (forM_)
import Data.Array.Base (MArray)
import qualified Data.Map as Map

import Direction (Direction(..))
import Connections (ClusterConnection)


type Connection = (Int, Int)


data Cluster =
    Cluster { -- | number of nodes in cluster
              nodesNum :: Int,
              -- | nodes connections
              adjList :: [Connection],
              -- | cluster sockets
              sockets :: Map Direction [Connection]
            } deriving (Show)

cluster = Cluster


append :: MArray a Int m => Cluster -> a (Int, Int) Int -> Int -> m ()
append (Cluster len conns _) matrix num = do
    let shift = len * num
    forM_ conns $ \ (i, j) -> do
        writeArray matrix (shift + i, shift + j) 1
        writeArray matrix (shift + j, shift + i) 1


connect :: MArray a Int m => Cluster -> a (Int, Int) Int -> ClusterConnection -> m ()
connect (Cluster len _ sockets) matrix (from, to, direction) = do
    let conns = sockets ! direction
    let fromShift = len * from
    let toShift = len * to
    forM_ conns $ \ (i, j) -> do
        writeArray matrix (fromShift + i, toShift + j) 1
        writeArray matrix (toShift + j, fromShift + i) 1


single :: Cluster
single = cluster
    1
    []
    (Map.fromList [(FromLeft,  [(1, 1)]),
                   (FromRight, [(1, 1)]),
                   (FromDown,  [(1, 1)]),
                   (FromUp,    [(1, 1)])])

second :: Cluster
second = cluster
    8
    [(1, 5), (1, 3),
     (2, 6), (2, 4),
     (3, 7),
     (4, 8),
     (5, 6), (5, 7), (5, 8),
     (6, 7), (6, 8),
     (7, 8)]
    (Map.fromList [(FromLeft,  [(2, 1), (3, 4)]),
                   (FromRight, [(1, 2), (4, 3)]),
                   (FromDown,  [(1, 4), (2, 3)]),
                   (FromUp,    [(4, 1), (3, 2)])])


square :: Cluster
square = cluster
    8
    [(1, 2), (2, 3), (3, 4), (4, 1),
     (1, 5), (2, 6), (3, 7), (4, 8),
     (5, 6), (6, 7), (7, 8), (8, 5)]
    (Map.fromList [(FromLeft, [(2, 1), (3, 4)]),
                   (FromRight, [(1, 2), (4, 3)]),
                   (FromDown, [(5, 8), (6, 7)]),
                   (FromUp, [(8, 5), (7, 6)])])
