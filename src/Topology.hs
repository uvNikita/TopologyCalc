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

import           Data.Array (Array)
import           Data.Array.ST (freeze)
import           Data.Matrix (Matrix)
import           Control.Monad.ST (runST)
import           Control.Monad (forM_)

import           Utils (newSTArray, fromArray)
import           Cluster (Cluster, append, connect)
import           Connections (Connections)
import qualified Connections
import qualified Cluster


data Topology = Topology Cluster Connections


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
