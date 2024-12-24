{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where

import Data.List (transpose)
import Data.Array
import qualified Data.Set as S
import qualified Data.Map as M

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (i, j) = [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]

main :: IO ()
main = interact solve

solve :: String -> String
solve s = unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s]

part1 :: String -> String
part1 s = show $ sum $ M.map S.size zeroes where
  m = readInput s
  (lv9:lvls) = getLevels m
  nines = M.fromSet S.singleton lv9
  zeroes = foldl descend nines lvls

descend :: M.Map (Int, Int) (S.Set (Int, Int)) -> S.Set (Int, Int) -> M.Map (Int, Int) (S.Set (Int, Int))
descend upper = M.fromSet (S.unions . map (flip (M.findWithDefault S.empty) upper) . neighbours)

part2 :: String -> String
part2 s = show (sum zeroes) where
  m = readInput s
  (lv9:lvls) = getLevels m
  nines = M.fromSet (const (1 :: Int)) lv9
  zeroes = foldl descend' nines lvls

descend' :: M.Map (Int, Int) Int -> S.Set (Int, Int) -> M.Map (Int, Int) Int
descend' upper = M.fromSet (sum . map (flip (M.findWithDefault 0) upper) . neighbours)

readInput :: String -> Array (Int, Int) Char
readInput s = listArray ((0, 0), (w - 1, h - 1)) (concat ls) where
  ls = transpose (lines s)
  w = length (head ls)
  h = length ls

getLevels :: Array (Int, Int) Char -> [S.Set (Int, Int)]
getLevels m = map (\c -> S.filter (\i -> m ! i == c) (S.fromList (indices m))) (reverse ['0' .. '9'])
