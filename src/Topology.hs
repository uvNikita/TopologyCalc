-----------------------------------------------------------------------------
--
-- Module      :  Topology
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

module Topology (
      Topology(..)
    , adjMatrix
    , adjArray
) where

import           Data.Array (Array, Ix)
import           Data.Array.ST (writeArray, newArray, freeze, STArray)
import           Data.Matrix (Matrix)
import qualified Data.Map as Map
import           Control.Monad.ST (runST, ST)
import           Control.Monad (forM_, mapM_)

import           Utils
import           Cluster (Cluster, append, connect)
import           Cluster.Second (second)
import           Connections (Connections)
import qualified Connections
import qualified Cluster
import           Connections.Tree (treeG)


data Topology = Topology Cluster Connections


newSTArray :: Ix i => (i,i) -> e -> ST s (STArray s i e)
newSTArray = newArray


adjArray :: Topology -> Array (Int, Int) Int
adjArray (Topology cluster conns) = runST $ do
    let clusterNum = Connections.clusterNum conns
    let nodesInCluster = Cluster.nodesNum cluster
    let nodesNum = clusterNum * nodesInCluster
    matrix <- newSTArray ((1, 1), (nodesNum, nodesNum)) 0
    mapM_ (append cluster matrix) [0 .. clusterNum - 1]
    forM_ (Connections.conns conns) (connect cluster matrix)
    freeze matrix


adjMatrix :: Topology -> Matrix Int
adjMatrix = fromArray . adjArray
