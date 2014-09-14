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
      TopologyGen(..)
    , Topology(..)
    , Direction(..)
) where

import Data.Array (Array, Ix)
import Data.Matrix (Matrix)
import Data.Array.ST (writeArray, newArray, freeze, STArray)
import Control.Monad (forM_, mapM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.ST (runST, ST)
import Utils

type Connection = (Int, Int)

data Direction = FromLeft | FromRight | FromUp | FromDown deriving (Show, Eq)

data Cluster = Cluster Int [Connection] (Map Direction [Connection])

testCluster = Cluster 3 [(0, 1), (1, 2)] undefined

test :: Array (Int, Int) Int
test = runST $ do
    matrix <- newSTArray ((0, 0), (10, 10)) 0
    mapM_ (addCluster testCluster matrix) [0, 2]
    freeze matrix
    where newSTArray :: Ix i => (i,i) -> e -> ST s (STArray s i e)
          newSTArray = newArray

addCluster (Cluster len cons _) matrix num = do
    let shift = len * num
    forM_ cons $ \ (i, j) ->
        writeArray matrix (shift + i, shift + j) 1

data Topology = Topology Int [(Int, Int, Direction)] deriving (Show)
newtype TopologyGen = TopologyGen { generate :: Int -> Topology }
