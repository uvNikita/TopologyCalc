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
    , nodesNum
    , adjList
    , sockets
) where

import Data.Map (Map)
import Data.Array.ST (writeArray, newArray, freeze, STArray)
import Control.Monad (forM_, mapM_)

import Direction (Direction(..))


type Connection = (Int, Int)


data Cluster =
    Cluster { -- | number of nodes in cluster
              nodesNum :: Int,
              -- | nodes connections
              adjList :: [Connection],
              -- | cluster sockets
              sockets :: (Map Direction [Connection])
            } deriving (Show)

cluster = Cluster

append (Cluster len cons _) matrix num = do
    let shift = len * num
    forM_ cons $ \ (i, j) ->
        writeArray matrix (shift + i, shift + j) 1
