-----------------------------------------------------------------------------
--
-- Module      :  Connections.Ring
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

module Connections.Ring (
    ringG
) where

import Connections
import Direction


ringG :: ConnectionsGen
ringG = ConnectionsGen ring'


ring' :: Int -> Connections
ring' levels = Connections levels cs
    where cs = outerRing levels ++ innerRing levels


outerRing :: Int -> [ClusterConnection]
outerRing levels = map connect neighbors
    where lastlvl = levels - 1
          neighbors = (lastlvl, 0) : zip [0 .. lastlvl] [1 .. lastlvl]
          connect (i, j) = (i, j, FromLeft)


innerRing :: Int -> [ClusterConnection]
innerRing levels = map connect neighbors
    where num = (levels + 1) `div` 2
          lastlvl = (num - 1) * 2
          neighbors = (lastlvl, 0) : zip [0, 2 .. lastlvl] [2, 4 .. lastlvl]
          connect (i, j) = (i, j, FromUp)
