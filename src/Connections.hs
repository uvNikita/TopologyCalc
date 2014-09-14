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
    , Connections(..)
    , Direction(..)
) where

import Direction (Direction(..))

type Connection = (Int, Int, Direction)

data Connections = Connections Int [Connection] deriving (Show)

newtype ConnectionsGen = ConnectionsGen { generate :: Int -> Connections }
