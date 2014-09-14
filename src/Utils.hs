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
) where

import Data.Array (bounds, elems, Array)
import Data.Matrix (Matrix, fromList)

fromArray :: Array (Int, Int) a -> Matrix a
fromArray array = fromList rows columns . elems $ array
    where ((i1, j1), (i2, j2)) = bounds array
          rows = i2 - i1 + 1
          columns = j2 - j1 + 1
