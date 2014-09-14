{-# LANGUAGE NamedFieldPuns #-}
-----------------------------------------------------------------------------
--
-- Module      :  Record
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

module Record (
      record
    , printRecords
) where


import Text.PrettyPrint.Boxes (Box, printBox, vcat, hsep, left, text)
import Data.List (transpose)


data Record = Record {
      diam :: Int
    , power :: Int
} deriving (Show)

record = Record


rowsToBox :: [[String]] -> Box
rowsToBox rows = box
    where box = hsep 2 left colsBox
          colsBox = map colToBox $ transpose rows
          colToBox = vcat left . map text

toRow :: Record -> [String]
toRow (Record {diam, power}) = map show [diam, power]

headRow = ["D", "P"]

recordsBox :: [Record] -> Box
recordsBox records = rowsToBox (headRow : map toRow records)

records = [Record 1 2, Record 3 4]

printRecords = printBox . recordsBox
