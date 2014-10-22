-----------------------------------------------------------------------------
--
-- Module      :  Connections.Mash
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

module Connections.Mash (
    mashG
) where

import Connections
import Direction

mashG :: ConnectionsGen
mashG = ConnectionsGen mash'

mash' :: Int -> Connections
mash' levels = Connections num (nexts ++ downs)
    where num = levels * levels
          connectNext i = (i
                         , if isLast i
                               then i + 1 - levels
                               else i + 1
                         , FromLeft)
          hasNext i = not (isLast i && (row i `mod` 2 /= 0))
          nexts = map connectNext . filter hasNext $ [0 .. n - 1]
          isLast i = (i + 1) `mod` levels == 0

          connectDown i = (i, (i + levels) `mod` n, FromUp)
          hasDown i = not (isDown i && (col i `mod` 2 == 0))
          downs = map connectDown . filter hasDown $ [0 .. n - 1]
          isDown i = i `elem` [n - levels .. n - 1]

          row i = i `div` levels
          col i = i `mod` levels

          n = levels * levels
