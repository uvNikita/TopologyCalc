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

module Tree (
    tree
) where

import Topology

tree :: TopologyGen
-- topology :: Int -> Int -> [(Int, Int, Direction)]
tree = TopologyGen tree'


tree' levels = Topology num cs
    where lastLevel = levels - 1
          num = 2 ^ lastLevel
          cs = concatMap connections [0..lastLevel]


bounds :: Int -> (Int, Int)
bounds level = (fid, lid)
    where fid = 2 ^ level - 1
          lid = 2 ^ (level + 1) - 2

parent id = (id - 1) `div` 2

connections 0 = []
connections level | even level = evenConns level
                  | otherwise = oddConns level

evenConns level = first : last : parents
    where [(ppfid, pplid), (pfid, plid), (fid, lid)] = map bounds [level - 2, level - 1, level]
          first = (fid, ppfid, FromLeft)
          last = (lid, pplid, FromRight)
          connParent id = (id, parent id, FromUp)
          parents = map connParent [fid .. lid]

oddConns level = inLevel ++ parents
    where (fid, lid) = bounds level
          inLevel = (fid, lid, FromLeft) : map connLeft [fid + 1 .. lid]
          parents = map connParent [fid .. lid]
          connLeft id = (id, id - 1, FromLeft)
          connParent id = (id, parent id, FromUp)
