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
    , cluster
    , append
    , connect
    , nodesNum
    , adjList
    , sockets
) where

import Data.Map (Map, (!))
import Data.Array.ST (writeArray, newArray, freeze, STArray)
import Control.Monad (forM_, mapM_)
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
