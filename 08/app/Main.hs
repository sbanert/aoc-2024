module Main where

import Data.Array
import Data.List (transpose)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

main :: IO ()
main = interact solve

solve :: String -> String
solve s = unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s]

part1 :: String -> String
part1 s = show (S.size (S.filter (inRange (bounds m)) (getAllAntinodes (getAntennas m)))) where
  m = getMap s

part2 :: String -> String
part2 s = show (S.size (getAllAntinodes' (bounds m) (getAntennas m))) where
  m = getMap s

getMap :: String -> Array (Int, Int) Char
getMap s = listArray ((0, 0), (w - 1, h - 1)) (concat ls) where
  ls = transpose (lines s)
  w = length (head ls)
  h = length ls

getAntennas :: Array (Int, Int) Char -> M.Map Char (S.Set (Int, Int))
getAntennas a = foldl addCharToMap M.empty (filter ((/= '.') . snd) (assocs a))

addCharToMap :: M.Map Char (S.Set (Int, Int)) -> ((Int, Int), Char) -> M.Map Char (S.Set (Int, Int))
addCharToMap m (i, c) = M.alter (addPosToSet i) c m

addPosToSet :: (Int, Int) -> Maybe (S.Set (Int, Int)) -> Maybe (S.Set (Int, Int))
addPosToSet i Nothing = Just (S.singleton i)
addPosToSet i (Just s) = Just (S.insert i s)

getAntinodes :: S.Set (Int, Int) -> S.Set (Int, Int)
getAntinodes as = S.map (\((x1, y1), (x2, y2)) -> (2 * x1 - x2, 2 * y1 - y2)) (S.filter (uncurry (/=)) (as `S.cartesianProduct` as))

getAllAntinodes :: M.Map Char (S.Set (Int, Int)) -> S.Set (Int, Int)
getAllAntinodes = M.foldl' (\acc antennas -> acc `S.union` getAntinodes antennas) S.empty

getAllAntinodes' :: ((Int, Int), (Int, Int)) -> M.Map Char (S.Set (Int, Int)) -> S.Set (Int, Int)
getAllAntinodes' bs = M.foldl' (\acc antennas -> acc `S.union` getAntinodes' bs antennas) S.empty

getAntinodes' :: ((Int, Int), (Int, Int)) -> S.Set (Int, Int) -> S.Set (Int, Int)
getAntinodes' bs as = S.foldl S.union S.empty antinodeSets where
  pairs = S.filter (uncurry (/=)) (as `S.cartesianProduct` as)
  antinodeSequences = S.map (\((x1, y1), (x2, y2)) -> antinodeSequence (x1, y1) (x2 - x1, y2 - y1)) pairs
  antinodeSets = S.map (S.fromList . takeWhile (inRange bs)) antinodeSequences

antinodeSequence :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
antinodeSequence (x, y) (dx, dy) = (x, y):antinodeSequence (x + dx, y + dy) (dx, dy)
