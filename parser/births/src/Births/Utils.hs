{-# LANGUAGE OverloadedStrings, KindSignatures #-}

module Births.Utils where

import Births.Data

import qualified Data.Text as T
import Control.Applicative
import Data.Maybe

-- | Split the crappy page into a crappy header, and the data. Perform some voodoo.
splitPage :: CrappyPdfToTextPage -> ChunkedPage
splitPage p = ChunkedPage { tableHeader = header_data, tableData = table_data }
  where

    splN = T.splitOn (T.pack "\n\n") 
    page_lines = splN p

    header_t = T.splitOn (T.pack "TOTAL BIRTHS") p
    header_length = (length $ splN (header_t !! 0)) - 1

    -- this is an array cleaned for whitespace
    header_data = multiwhtsplit <$> take header_length page_lines
    table_data = T.words <$> drop header_length page_lines

    -- Split by multiple whitespaces to preserve multi-word elements containing
    -- only one space
    multiwhtsplit :: T.Text -> [T.Text]
    multiwhtsplit t = cols
      where
        splitted = T.splitOn (T.pack "  ") t
        cols = T.strip <$> [b | b <- splitted, T.length b > 0]

seekRowContains :: T.Text -> [[T.Text]] -> [T.Text]
seekRowContains text rows = row
  where row = [r | r <- rows, text `T.isInfixOf` T.intercalate " " r] !! 0

-- | get a row containing a text string
getRow :: [[T.Text]] -> T.Text -> [T.Text]
getRow rows query = row
  where
    row = drop query_length $ (query `seekRowContains` rows)
    query_length = length $ T.splitOn (T.pack " ") query

-- | Split pages on line feed
splitPages ::  T.Text -> [T.Text]
splitPages t = T.splitOn (T.pack "\f") t

-- | Throw out pages not containing the tabular data
filterPages :: [T.Text] -> [T.Text]
filterPages ps = 
    [ tp | tp <- ps
    , "TOTAL BIRTHS" `T.isInfixOf` tp]

