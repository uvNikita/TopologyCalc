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

module Utils (
      fromArray
    , newSTArray
) where

import Data.Array (bounds, elems, Array, Ix)
import Data.Array.ST (STArray, newArray)
import Data.Matrix (Matrix, fromList)

import Control.Monad.ST (ST)


newSTArray :: Ix i => (i,i) -> e -> ST s (STArray s i e)
newSTArray = newArray


fromArray :: Array (Int, Int) a -> Matrix a
fromArray array = fromList rows columns . elems $ array
    where ((i1, j1), (i2, j2)) = bounds array
          rows = i2 - i1 + 1
          columns = j2 - j1 + 1
