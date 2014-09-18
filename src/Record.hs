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
      printRecords
    , Record (..)
) where


import Text.PrettyPrint.Boxes (Box, printBox, vcat, hsep, left, text)
import Data.List (transpose)


data Record = Record {
      nodes :: Int
    , power :: Float
    , diam :: Float
    , avgDiam :: Float
    , traffic :: Float
    , cost :: Float
} deriving (Show)


rowsToBox :: [[String]] -> Box
rowsToBox rows = box
    where box = hsep 2 left colsBox
          colsBox = map colToBox $ transpose rows
          colToBox = vcat left . map text

toRow :: Record -> [String]
toRow rec = show (nodes rec) : map show [power rec, diam rec, avgDiam rec, traffic rec, cost rec]

headRow = ["N", "S", "D", "avg(D)", "T", "C"]

recordsBox :: [Record] -> Box
recordsBox records = rowsToBox (headRow : map toRow records)

printRecords = printBox . recordsBox
