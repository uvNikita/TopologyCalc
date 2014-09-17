-----------------------------------------------------------------------------
--
-- Module      :  Analysis
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

module Analysis (
    minDistance
) where


import           Data.Array (Array, elems)
import           Data.Array.ST (writeArray, readArray, freeze)
import           Data.Matrix (Matrix, nrows, getRow, fromLists)
import           Data.Vector (foldM, findIndices)
import qualified Data.Vector as V
import qualified Data.Set as Set

import           Control.Monad.ST (runST, ST)
import           Control.Parallel.Strategies (parMap, rdeepseq)

import           Utils (newSTArray)


minDistance :: Matrix Int -> Matrix Int
minDistance adjMatrix = fromLists rows
    where rows = map' (elems . dijkstra adjMatrix) [1 .. nrows adjMatrix]
          map' = parMap rdeepseq


dijkstra :: Matrix Int -> Int -> Array Int Int
dijkstra adjMatrix src = runST $ do
    let num = nrows adjMatrix
    minDist <- newSTArray (1, num) maxBound
    writeArray minDist src 0
    let neighbors u = V.map (+ 1) indices
                      where row = getRow u adjMatrix
                            indices = findIndices (/= 0) row
    let loop queue = case Set.minView queue of
            Nothing -> return ()
            Just ((dist, u), queue') -> foldM step queue' (neighbors u) >>= loop
                where new = dist + 1
                      step queue v = do
                        old <- readArray minDist v
                        if new >= old
                            then
                                return queue
                            else do
                                let queue' = Set.delete (old, v) queue
                                writeArray minDist v new
                                return $ Set.insert (new, v) queue'
    loop (Set.singleton (0, src))
    freeze minDist
