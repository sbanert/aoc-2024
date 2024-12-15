module Main where

import Data.Array
import Data.Array.ST
import Data.List (transpose)
import Control.Monad (forM_, when, unless)
import Data.STRef

data Direction = U | D | L | R deriving Show
data Tile = Wall | Box | Robot | Free deriving (Show, Eq)
newtype Board = Board (Array (Int, Int) Tile)

instance Show Board where
  show (Board m) = unlines ([]:map (map (tile2Char . (m !))) is) where
    is = [[(i, j) | i <- [x0 .. x1]] | j <- [y0 .. y1]]
    ((x0, y0), (x1, y1)) = bounds m

main :: IO ()
main = interact solve

solve :: String -> String
solve s = unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s]

part1 :: String -> String
part1 = show . totalGPS . simulate . readInput

totalGPS :: Board -> Int
totalGPS (Board b) = sum $ map gps $ filter ((== Box) . (b !)) (indices b)

gps :: (Int, Int) -> Int
gps (x, y) = x + 100 * y

part2 :: String -> String
part2 = const ""

lookTowards :: Direction -> (Int, Int) -> [(Int, Int)]
lookTowards d (x, y) = goTowards d (x, y) : lookTowards d (goTowards d (x, y))

goTowards :: Direction -> (Int, Int) -> (Int, Int)
goTowards U (x, y) = (x, y - 1)
goTowards D (x, y) = (x, y + 1)
goTowards L (x, y) = (x - 1, y)
goTowards R (x, y) = (x + 1, y)

simulate :: (Board, [Direction], (Int, Int)) -> Board
simulate (Board m, ds, pos) = Board $ runSTArray $ do
    m' <- thaw m
    pos' <- newSTRef pos
    writeArray m' pos Free
    forM_ ds $ \d -> do
      p <- readSTRef pos'
      bds <- getBounds m'
      let nextPos = takeWhile (inRange bds) (lookTowards d p)
      nextTiles <- mapM (readArray m') nextPos
      let (posAfterBoxes, tileAfterBoxes) = head $ dropWhile ((== Box) . snd) (zip nextPos nextTiles)
      unless (tileAfterBoxes == Wall) (modifySTRef pos' (goTowards d))
      when (tileAfterBoxes /= Wall && posAfterBoxes /= goTowards d p) (writeArray m' posAfterBoxes Box >> writeArray m' (goTowards d p) Free)
    lastPos <- readSTRef pos'
    writeArray m' lastPos Robot
    return m'

readInput :: String -> (Board, [Direction], (Int, Int))
readInput s = (Board floorMap, directions, robotPos) where
  ls = lines s
  (mapTransposed, dirChars) = span (/= []) ls
  w = length mapTransposed
  h = length (head mapTransposed)
  floorMap = listArray ((0, 0), (w - 1, h - 1)) (map char2Tile $ concat (transpose mapTransposed))
  directions = map char2Dir (concat dirChars)
  robotPos = head $ filter (\i -> floorMap ! i == Robot) (indices floorMap)

char2Dir :: Char -> Direction
char2Dir '>' = R
char2Dir '<' = L
char2Dir '^' = U
char2Dir 'v' = D
char2Dir _ = undefined

char2Tile :: Char -> Tile
char2Tile 'O' = Box
char2Tile '.' = Free
char2Tile '#' = Wall
char2Tile '@' = Robot
char2Tile _ = undefined

tile2Char :: Tile -> Char
tile2Char Box = 'O'
tile2Char Free = '.'
tile2Char Wall = '#'
tile2Char Robot = '@'
