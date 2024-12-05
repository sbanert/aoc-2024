{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

import Data.Array
import Data.List (sortBy)

main :: IO ()
main = interact solve

solve :: String -> String
solve s = unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s]

part1 :: String -> String
part1 = show . sum . map middleElements . extractValidUpdates . parseInput

part2 :: String -> String
part2 s = show (sum (map middleElements sortedInvalidUpdates)) where
  (rules, updates) = parseInput s
  om = makeOrderingMatrix rules
  invalidUpdates = extractInvalidUpdates (rules, updates)
  sortedInvalidUpdates = map (sortBy (customOrd om)) invalidUpdates

-- ((11, 11), (99, 99))

makeOrderingMatrix :: [(Int, Int)] -> Array (Int, Int) Bool
makeOrderingMatrix rs = mf where
  mf = accumArray (||) False ((11, 11), (99, 99)) (map (\r -> (r, True)) rs)

customOrd :: Array(Int, Int) Bool -> Int -> Int -> Ordering
customOrd m a b
  | a == b = EQ
  | m ! (a, b) = LT
  | m ! (b, a) = GT
  | otherwise = LT

middleElements :: [Int] -> Int
middleElements els = els !! (length els `div` 2)

extractInvalidUpdates :: ([(Int, Int)], [[Int]]) -> [[Int]]
extractInvalidUpdates (rules, updates) = filter (not . isValidUpdate) updates where
  isValidUpdate :: [Int] -> Bool
  isValidUpdate l = all (satisfiesRule l) rules
   
extractValidUpdates :: ([(Int, Int)], [[Int]]) -> [[Int]]
extractValidUpdates (rules, updates) = filter isValidUpdate updates where
  isValidUpdate :: [Int] -> Bool
  isValidUpdate l = all (satisfiesRule l) rules

satisfiesRule :: [Int] -> (Int, Int) -> Bool
satisfiesRule up (a, b) = filteredUpdate /= [b, a] where
  filteredUpdate = filter (\x -> x == a || x == b) up

parseInput :: String -> ([(Int, Int)], [[Int]])
parseInput s = (pairs, updates) where
  (pairLines, updateLines) = span (/= "") (lines s)
  pairs = map extractPair pairLines
  updates = map extractUpdate (tail updateLines)

extractPair :: String -> (Int, Int)
extractPair s = (a, b) where
  (x, y) = span (/= '|') s
  a = read x
  b = read (tail y)

mapCommaToSpace :: Char -> Char
mapCommaToSpace ',' = ' '
mapCommaToSpace c = c

extractUpdate :: String -> [Int]
extractUpdate s = map read (words (map mapCommaToSpace s))
