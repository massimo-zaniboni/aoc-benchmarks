
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

getInputArray :: Int -> IO (A.UArray Coord Char)
getInputArray day = makeArray <$> getInputLines id day
  where
    makeArray rows =
      A.listArray bounds (concat rows)
        where
          height = length rows
          width  = length (head rows)
          bounds = ( origin, C (height-1) (width-1) )

count :: (a -> Bool) -> [a] -> Int
count p xs = foldl' f 0 xs
  where
    f n x | p x = n+1 | otherwise = n

freqs :: (Ord a) => [a] -> [(a,Int)]
freqs xs = combine [ (x,1) | x <- xs ]
  where
    combine = M'.toAscList . M'.fromListWith (+)

-- Coords

-- | Row-major coordinates
data Coord = C !Int !Int
  deriving (Read, Show, Eq, Ord)

origin, north, east, south, west :: Coord
origin = C 0 0
north  = C (-1) 0
east   = C 0 1
south  = C 1 0
west   = C 0 (-1)

coordRow, coordCol :: Coord -> Int
coordRow (C row _) = row
coordCol (C _ col) = col

invert :: Coord -> Coord
invert (C y x) = C x y

above, right, below, left :: Coord -> Coord
above (C y x) = C (y-1) x
right (C y x) = C y (x+1)
below (C y x) = C (y+1) x
left  (C y x) = C y (x-1)

turnLeft, turnRight, turnAround :: Coord -> Coord
turnLeft   (C y x) = C (-x) y
turnRight  (C y x) = C x (-y)
turnAround (C y x) = C (-y) (-x)

cardinal :: Coord -> [Coord]
cardinal c = c `seq`
  [above c, right c, below c, left c]

neighbors :: Coord -> [Coord]
neighbors c = c `seq`
  [ above c, left c, right c, below c
  , above (left c), above (right c)
  , below (left c), below (right c) ]

bookreading :: Coord -> [Coord]
bookreading c = c `seq`
  [ above (left c), above c, above (right c)
  , left c        , c      , right c
  , below (left c), below c, below (right c) ]

manhattan :: Coord -> Coord -> Int
manhattan (C y x) (C v u) = abs (y-v) + abs (x-u)

addCoord :: Coord -> Coord -> Coord
addCoord (C y x) (C v u) = C (y+v) (x+u)

scaleCoord :: Int -> Coord -> Coord
scaleCoord n (C y x) = C (n*y) (n*x)

instance Ix Coord where
  unsafeIndex (C ym xm,C _yM xM) (C y x) =
    (y - ym) * (xM - xm + 1) + (x - xm)

  inRange (C ym xm,C yM xM) (C y x) =
    ym <= y && y <= yM && xm <= x && x <= xM

  range (C ym xm,C yM xM) =
    [ C y x | y <- [ym,ym+1..yM], x <- [xm,xm+1..xM] ]

boundingBox :: Foldable t => t Coord -> Maybe (Coord,Coord)
boundingBox t =
  case toList t of
    []         -> Nothing
    C y x : cs -> Just $ go x y x y cs
    where
      go xm ym xM yM [] = (C ym xm,C yM xM)
      go xm ym xM yM (C y x : cs) =
        go (min xm x) (min ym y) (max xM x) (max yM y) cs

drawCoords :: Map Coord Char -> String
drawCoords pixels =
  unlines [ [ pixel (C y x) | x <- [xm..xM]] | y <- [ym..yM] ]
  where
    pixel c = M.findWithDefault ' ' c pixels
    Just (C ym xm,C yM xM) = boundingBox (M.keys pixels)

showCoords :: Foldable t => t Coord -> String
showCoords t = drawCoords $ M.fromList [ (c,'#') | c <- toList t ]

withCoords :: (Char -> a) -> [String] -> [(Coord,a)]
withCoords f rows = concat $
  [ [ (C y x, f c) | (x,c) <- zip [0..] xs ] | (y,xs) <- zip [0..] rows ]
