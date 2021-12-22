{-# LANGUAGE ScopedTypeVariables #-}

-- author: giorgio
-- note: written in a hurry during the contest

module Main where

import System.IO
import qualified Data.Map as M
import System.Environment

-- State represent the fishes' state
type State = M.Map Int Integer
-- a state :: State is such that
-- lookup i state represent the number of fishes
-- that will reproduce in i-days.

start :: State
start = M.fromAscList [(i,0) | i<- [0..8]]

readInput :: [Int] -> State
readInput = ausil start
  where
    ausil acc [] = acc
    ausil acc (fish:fishes) =
      ausil (M.update (\x -> Just (x+1)) fish acc) fishes

($>) :: a -> (a -> b) -> b
x $> f = f x

-- unsafe extraction of values from State
extract :: State -> Int -> Integer
extract state n =
  case (M.lookup n state) of
    Just val -> val

update :: State -> State
update state =
  M.empty $>
  M.insert 0 (extract state 1) $>
  M.insert 1 (extract state 2) $>
  M.insert 2 (extract state 3) $>
  M.insert 3 (extract state 4) $>
  M.insert 4 (extract state 5) $>
  M.insert 5 (extract state 6) $>
  M.insert 6 ((extract state 7)+(extract state 0)) $>
  M.insert 7 (extract state 8) $>
  M.insert 8 (extract state 0)

iter :: Int -> (a -> a) -> a -> a
iter 0 f init = init
iter n f init = iter (n-1) f (f init)

summer :: State -> Integer
summer state = foldl (\acc -> \index -> acc + (extract state index)) 0 [0..8]

main :: IO()
main = do
  raw <- getLine
  let list = read ('[':raw++"]") :: [Int]
  let init = readInput list
  iterCountS <- getLine
  let iterCount :: Int = read iterCountS
  let val = iter iterCount update init
  print (summer val)
