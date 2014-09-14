-----------------------------------------------------------------------------
--
-- Module      :  Connections.Tree
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

module Connections.Tree (
    treeG
) where

import Connections

treeG :: ConnectionsGen
treeG = ConnectionsGen tree'


tree' levels = Connections num cs
    where lastLevel = levels - 1
          num = 2 ^ levels - 1
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
