
{-# LANGUAGE TypeApplications #-}

-- SPDX-License-Identifier: ISC
-- Copyright (c) 2021 Paolo Martini <mrtnpaolo@protonmail.com>

module Main (main) where

import Data.List (foldl')
import Data.Map.Strict qualified as M' (toAscList,fromListWith)

main =
  do crabs <- getInput parse 7
     print (part2 crabs)
  where
    parse xs = read @[Int] $ '[':xs++"]"

optimal cost xs = minimum (map cost [minimum xs..maximum xs])

part2 xs = optimal (\x -> sum [ triangle (abs (n-x)) | n <- xs]) xs
  where
    triangle n = n*(n+1)`div`2

-- Utilities

getInput :: (String -> a) -> Int -> IO a
getInput parse day = parse <$>  getContents
