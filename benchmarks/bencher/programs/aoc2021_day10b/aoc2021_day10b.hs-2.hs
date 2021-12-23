
-- SPDX-License-Identifier: ISC
-- Copyright (c) 2021 Paolo Martini <mrtnpaolo@protonmail.com>

module Main (main) where

import Data.List   (foldl',sort)
import Data.Either (partitionEithers)

import Data.List (foldl')
import Data.Map.Strict qualified as M' (toAscList,fromListWith)

main =
  do (corrupted,incomplete) <- partitionEithers <$> getInputLines parse 10
     print (part2 incomplete)

parse = go []
  where
    go seen [] = Right seen           -- string is incomplete

    go seen@(~(s:rest)) (x:xs)
      | open x       = go (x:seen) xs
      | s == match x = go    rest  xs
      | otherwise    = Left x         -- string is corrupted

open x = x `elem` "([{<"

match ')' = '('
match ']' = '['
match '}' = '{'
match '>' = '<'

part2 = middle . map (base5 . map score)
 where
    base5 = foldl' (\a i -> a*5 + i) 0
    middle xs = sort xs !! (length xs `div` 2)

score '(' = 1; score ')' = 3
score '[' = 2; score ']' = 57
score '{' = 3; score '}' = 1197
score '<' = 4; score '>' = 25137


-- Utilities

getInput :: (String -> a) -> Int -> IO a
getInput parse day = parse <$>  getContents

getInputLines :: (String -> a) -> Int -> IO [a]
getInputLines parse day = getInput (map parse . lines) day

