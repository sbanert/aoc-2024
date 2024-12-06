module Main where

import Data.Array
import Data.List (transpose)
import qualified Data.Set as S

main :: IO ()
main = interact solve

data MapSymbol = Empty | Obstacle | Guard deriving (Show, Eq)
data Direction = U | D | L | R deriving (Eq, Ord)

nextDirection :: Direction -> Direction
nextDirection U = R
nextDirection R = D
nextDirection D = L
nextDirection L = U

solve :: String -> String
solve s = unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s]

part1 :: String -> String
part1 = show . S.size . guardPositions U S.empty . readInput

part2 :: String -> String
part2 s = show (S.size (S.filter (loops U S.empty (guard_pos, inmap)) possibleAdditionalObstacles)) where
  (guard_pos, inmap) = readInput s
  possibleAdditionalObstacles = guardPositions U S.empty (guard_pos, inmap)

loops :: Direction -> S.Set (Direction, (Int, Int)) -> ((Int, Int), Array (Int, Int) MapSymbol) -> (Int, Int) -> Bool
loops dir visited (guard_pos, inmap) addObst = ans where
  new_visited = S.insert (dir, guard_pos) visited
  pos_attempt = moveGuard dir guard_pos
  ans | not (inRange (bounds inmap) pos_attempt) = False
      | (dir, guard_pos) `S.member` visited = True
      | inmap ! pos_attempt == Obstacle || pos_attempt == addObst = loops (nextDirection dir) new_visited (guard_pos, inmap) addObst
      | otherwise = loops dir new_visited (pos_attempt, inmap) addObst

readInput :: String -> ((Int, Int), Array (Int, Int) MapSymbol)
readInput s = (guard_pos, inmap) where
  ls = transpose (lines s)
  w = length (head ls)
  h = length ls
  guard_pos = head (filter (\i -> (inmap ! i) == Guard) (indices inmap))
  inmap = listArray ((0, 0), (w-1, h-1)) (map syms (concat ls))

moveGuard :: Direction -> (Int, Int) -> (Int, Int)
moveGuard U (x, y) = (x, y-1)
moveGuard R (x, y) = (x+1, y)
moveGuard D (x, y) = (x, y+1)
moveGuard L (x, y) = (x-1, y)

guardPositions :: Direction -> S.Set (Int, Int) -> ((Int, Int), Array (Int, Int) MapSymbol) -> S.Set (Int, Int)
guardPositions dir positions (guard_pos, inmap) = ans where
  new_positions = S.insert guard_pos positions
  pos_attempt = moveGuard dir guard_pos
  ans | not (inRange (bounds inmap) pos_attempt) = new_positions
      | inmap ! pos_attempt == Obstacle = guardPositions (nextDirection dir) new_positions (guard_pos, inmap)
      | otherwise = guardPositions dir new_positions (pos_attempt, inmap)

syms :: Char -> MapSymbol
syms '#' = Obstacle
syms '^' = Guard
syms '.' = Empty
syms _ = undefined
