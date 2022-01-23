
-- SPDX-License-Identifier: ISC
-- Copyright (c) 2021 Paolo Martini <mrtnpaolo@protonmail.com>

module Main (main) where

import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Data.Array.Unboxed as A

import Numeric
import Data.List (transpose)

import Data.Ix
import GHC.Arr
import Data.Map (Map)
import qualified Data.Map as M
import Data.Foldable (toList)

import Data.List (foldl')
import Data.Map.Strict qualified as M' (toAscList,fromListWith)

main =
  do inp <- getInput lines 3
     print (part2 inp)

part2 xs = o₂ * co₂
  where
    o₂  = sieve 0 most  xs
    co₂ = sieve 0 least xs

    sieve _ _ [x] = toDecimal x
    sieve n p xs  = sieve (n+1) p xs'
      where
        digit = p (map (!! n) xs)
        xs'   = filter (\x -> digit == x !! n) xs

most xs
  | count ('0'==) xs > count ('1'==) xs = '0'
  | otherwise                           = '1'

least xs
  | most xs == '0' = '1'
  | otherwise      = '0'

toDecimal xs = let [(n,_)] = readBin xs in n

-- Utilities

getInput :: (String -> a) -> Int -> IO a
getInput parse day = parse <$>  getContents

getInputLines :: (String -> a) -> Int -> IO [a]
getInputLines parse day = getInput (map parse . lines) day

count :: (a -> Bool) -> [a] -> Int
count p xs = foldl' f 0 xs
  where
    f n x | p x = n+1 | otherwise = n


