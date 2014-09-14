a -----------------------------------------------------------------------------
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


second = Cluster 3 [(0, 1), (1, 2)] undefined
