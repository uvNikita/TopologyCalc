-----------------------------------------------------------------------------
--
-- Module      :  Direction
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

module Direction (
    Direction(..)
) where


data Direction = FromLeft | FromRight | FromUp | FromDown deriving (Show, Eq, Ord)
