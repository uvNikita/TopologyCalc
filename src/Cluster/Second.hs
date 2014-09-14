-----------------------------------------------------------------------------
--
-- Module      :  Cluster.Second
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

module Cluster.Second (
    second
) where

import Direction (Direction(..))
import Cluster
import Data.Map (fromList)

second = cluster
    8
    [(0, 4), (0, 2),
     (1, 5), (1, 3),
     (2, 6),
     (3, 7),
     (4, 5), (4, 6), (4, 7),
     (5, 6), (5, 7),
     (6, 7)]
    (fromList [(FromLeft,  [(1, 0), (2, 3)]),
               (FromRight, [(0, 1), (3, 2)]),
               (FromDown,  [(0, 3), (1, 2)]),
               (FromUp,    [(3, 0), (2, 1)])])
