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

tree' :: Int -> Connections
tree' levels = Connections num cs
    where lastLevel = levels - 1
          num = 2 ^ levels - 1
          cs = concatMap connections [0..lastLevel]


bounds :: Int -> (Int, Int)
bounds level = (fid, lid)
    where fid = 2 ^ level - 1
          lid = 2 ^ (level + 1) - 2


parent :: Integral a => a -> a
parent idx = (idx - 1) `div` 2


connections :: Int -> [ClusterConnection]
connections 0 = []
connections level | even level = evenConns level
                  | otherwise = oddConns level


evenConns :: Int -> [ClusterConnection]
evenConns level = fc : lc : parents
    where [(ppfid, pplid), (fid, lid)] = map bounds [level - 2, level]
          fc = (fid, ppfid, FromDown)
          lc = (lid, pplid, FromDown)
          connParent idx = (parent idx, idx, FromUp)
          parents = map connParent [fid .. lid]


oddConns :: Int -> [ClusterConnection]
oddConns level = inLevel ++ parents
    where (fid, lid) = bounds level
          inLevel = (lid, fid, FromLeft) : map connLeft [fid + 1 .. lid]
          parents = map connParent [fid .. lid]
          connLeft idx = (idx - 1, idx, FromLeft)
          connParent idx = (parent idx, idx, FromUp)
