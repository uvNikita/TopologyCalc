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
) where

import Data.Map (Map)
import Data.Array.ST (writeArray, newArray, freeze, STArray)
import Control.Monad (forM_, mapM_)

import Direction (Direction(..))


type Connection = (Int, Int)


data Cluster = Cluster Int [Connection] (Map Direction [Connection]) deriving (Show)

cluster num conns sockets = Cluster num conns sockets

append (Cluster len cons _) matrix num = do
    let shift = len * num
    forM_ cons $ \ (i, j) ->
        writeArray matrix (shift + i, shift + j) 1
