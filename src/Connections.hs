-----------------------------------------------------------------------------
--
-- Module      :  Connections
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

module Connections (
      ConnectionsGen(..)
    , Connections (..)
    , ClusterConnection
) where

import Direction (Direction(..))

type ClusterConnection = (Int, Int, Direction)

data Connections =
    Connections { -- | number of clusters
                  clusterNum :: Int,
                  -- | intercluster connections
                  conns :: [ClusterConnection]
                } deriving (Show)

newtype ConnectionsGen = ConnectionsGen { generate :: Int -> Connections }
