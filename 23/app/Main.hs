module Main where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

data Comp = Comp Char Char deriving (Eq, Ord)

instance Show Comp where
  show (Comp c1 c2) = [c1, c2]

main :: IO ()
main = interact solve

solve :: String -> String
solve s = unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s]

part1 :: String -> String
part1 s = show $ sum $ M.map S.size $ tTriples tconns conns where
  tconns = tConnections $ readInput s
  conns = S.fromList $ readInput s

part2 :: String -> String
part2 s = show $ S.toAscList (S.unions clNodes) where
  conns = S.fromList $ readInput s
  nodes = allNodes conns
  neighs = S.map (neighbours conns) nodes
  allButOneNeighbours = S.unions $ S.map removeOne neighs
  clNodes = S.filter (isClique conns) allButOneNeighbours

removeOne :: Ord a => S.Set a -> S.Set (S.Set a)
removeOne s = S.map (`S.delete` s) s

isClique :: S.Set (Comp, Comp) -> S.Set Comp -> Bool
isClique conns nodes = all (\p -> uncurry (==) p || p `S.member` conns || swap p `S.member` conns) pairs where
  pairs = nodes `S.cartesianProduct` nodes

readInput :: String -> [(Comp, Comp)]
readInput = map readConnection . lines

readConnection :: String -> (Comp, Comp)
readConnection [c1, c2, '-', c3, c4] = (Comp c1 c2, Comp c3 c4)
readConnection _ = undefined

tConnections :: [(Comp, Comp)] -> M.Map Char (S.Set Comp)
tConnections = foldl addConnection M.empty where
  addConnection m (Comp 't' c1, Comp 't' c2) = M.alter (mapSetInsert (Comp 't' (min c1 c2))) (max c1 c2) m
  addConnection m (Comp 't' c1, co) = M.alter (mapSetInsert co) c1 m
  addConnection m (co, Comp 't' c2) = M.alter (mapSetInsert co) c2 m
  addConnection m _ = m

tTriples :: M.Map Char (S.Set Comp) -> S.Set (Comp, Comp) -> M.Map Char (S.Set (Comp, Comp))
tTriples m conns = M.fromList [(c, getTriples (fromMaybe S.empty (M.lookup c m))) | c <- ['a' .. 'z']] where
  getTriples s = S.filter (`S.member` conns) (s `S.cartesianProduct` s)

mapSetInsert :: Comp -> Maybe (S.Set Comp) -> Maybe (S.Set Comp)
mapSetInsert co Nothing = Just (S.singleton co)
mapSetInsert co (Just s) = Just (S.insert co s)

allNodes :: S.Set (Comp, Comp) -> S.Set Comp
allNodes = S.foldl (\s (c1, c2) -> S.insert c1 (S.insert c2 s)) S.empty

neighbours :: S.Set (Comp, Comp) -> Comp -> S.Set Comp
neighbours conns n = foldl (\s (c1, c2) -> (if c1 == n then S.insert c2 s else if c2 == n then S.insert c1 s else s)) S.empty conns
