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
    , tree
) where

import Data.Array (Array, Ix)
import Data.Array.ST (writeArray, newArray, freeze, STArray)
import Data.Matrix (Matrix)
import qualified Data.Map as Map
import Control.Monad.ST (runST, ST)

import Utils
import Cluster (Cluster, append)
import Cluster.Second (second)
import Connections (ConnectionsGen)
import Connections.Tree (treeG)


data Topology = Topology Cluster ConnectionsGen

tree = Topology second treeG

test :: Array (Int, Int) Int
test = runST $ do
    matrix <- newSTArray ((0, 0), (10, 10)) 0
    mapM_ (append second matrix) [0, 2]
    freeze matrix
    where newSTArray :: Ix i => (i,i) -> e -> ST s (STArray s i e)
          newSTArray = newArray
