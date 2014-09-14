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
    [(1, 5), (1, 3),
     (2, 6), (2, 4),
     (3, 7),
     (4, 8),
     (5, 6), (5, 7), (5, 8),
     (6, 7), (6, 8),
     (7, 8)]
    (fromList [(FromLeft,  [(2, 1), (3, 4)]),
               (FromRight, [(1, 2), (4, 3)]),
               (FromDown,  [(1, 4), (2, 3)]),
               (FromUp,    [(4, 1), (3, 2)])])
