-- 13 + c * 101 = 65 + d * 103 
-- i mod 101 = 13
-- i mod 103 = 65
module Main where

import qualified Data.Set as S

wh :: (Int, Int)
-- wh = (11, 7)
wh = (101, 103)

nimg :: Int
nimg = head $ filter ((== 13) . (`mod` 101)) [65, 168 ..]

main :: IO ()
main = do
  s <- getContents
  putStrLn ("Part 1: " ++ part1 s)
  -- mapM_ (\i -> writeFile (show i ++ ".pbm") (toPbm $ map (simulate i) $ getRobots s)) [0 .. 999]
  writeFile (show nimg ++ ".pbm") (toPbm $ map (simulate nimg) $ getRobots s)

toPbm :: [(Int, Int)] -> String
toPbm l = unlines ["P1", unwords [show w, show h]] ++ img where
  (w, h) = wh
  pixels = S.fromList l
  indices = [[(i, j) | i <- [0 .. w-1]] | j <- [0 .. h-1]]
  img = unlines $ map (unwords . map toPixel) indices
  toPixel (i, j) = if (i, j) `S.member` pixels then "1" else "0"

-- solve :: String -> String
-- solve s = unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s]

part1 :: String -> String
part1 = show . safetyFactor . map (quadrant . simulate 100) . getRobots

-- part2 :: String -> String
-- part2 = const ""

parseRobot :: String -> ((Int, Int), (Int, Int))
parseRobot s = ((read x', read y'), (read dx', read dy')) where
  r1 = drop 2 s
  (x', r2) = span (/= ',') r1
  (y', r3) = span (/= ' ') (tail r2)
  (dx', r4) = span (/= ',') (drop 3 r3)
  dy' = tail r4

getRobots :: String -> [((Int, Int), (Int, Int))]
getRobots = map parseRobot . lines

simulate :: Int -> ((Int, Int), (Int, Int)) -> (Int, Int)
simulate n ((x, y), (dx, dy)) = ((x + n* dx) `mod` w, (y + n * dy) `mod` h) where (w, h) = wh

quadrant :: (Int, Int) -> Maybe Int
quadrant (x, y) | x < w `div` 2 && y < h `div` 2 = Just 1
  | x > w `div` 2 && y < h `div` 2 = Just 2
  | x < w `div` 2 && y > h `div` 2 = Just 3
  | x > w `div` 2 && y > h `div` 2 = Just 4
  | otherwise = Nothing where (w, h) = wh

safetyFactor :: [Maybe Int] -> Int
safetyFactor l = product $ map (\i -> length (filter (== Just i) l)) [1, 2, 3, 4]

