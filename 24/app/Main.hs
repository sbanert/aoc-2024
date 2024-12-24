module Main where

import qualified Data.Map as M
import qualified Data.Set as S

newtype Wire = Wire String deriving (Eq, Ord, Show)
data Gate = Gate Op Wire Wire deriving (Eq, Ord, Show)
data Op = And | Or | Xor deriving (Eq, Ord, Show)
data Output = Zero | One deriving Show

combine :: Op -> Output -> Output -> Output
combine And One One = One
combine And _ _ = Zero
combine Or Zero Zero = Zero
combine Or _ _ = One
combine Xor One One = Zero
combine Xor Zero Zero = Zero
combine Xor _ _ = One

main :: IO ()
main = interact solve

solve :: String -> String
solve s = unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s]

part1 :: String -> String
part1 s = show $ toInt $ map (fst evalZ M.!) zWires where
  (m0, m') = readInput s
  allWires = M.keysSet m0 `S.union` M.keysSet m'
  zWires = S.toAscList $ S.filter (\(Wire w) -> head w == 'z') allWires
  evalZ = foldl (flip forceEval) (m0, m') zWires

toInt :: [Output] -> Int
toInt [] = 0
toInt (Zero:r) = 2 * toInt r
toInt (One:r) = 1 + 2 * toInt r

-- toInt :: [Output] -> Int
-- toInt = toIntAux 0 where
--   toIntAux acc [] = acc
--   toIntAux acc (Zero:r) = toIntAux (2 * acc) r
--   toIntAux acc (One:r) = toIntAux (2 * acc + 1) r

forceEval :: Wire -> (M.Map Wire Output, M.Map Wire Gate) -> (M.Map Wire Output, M.Map Wire Gate)
forceEval w (m0, m') | M.member w m0 = (m0, m')
                     | otherwise = let
                       Gate o w1 w2 = m' M.! w
                       (m1, m'') = forceEval w1 (m0, m')
                       (m2, m''') = forceEval w2 (m1, m'')
                     in (M.insert w (combine o (m2 M.! w1) (m2 M.! w2)) m2, M.delete w m''')

part2 :: String -> String
part2 = const ""

readInput :: String -> (M.Map Wire Output, M.Map Wire Gate)
readInput s = (M.fromList inits , M.fromList gates) where
  ls = lines s
  (initLines, gateLines) = span (/= "") ls
  inits = map readInit initLines
  gates = map readGate (tail gateLines)

readInit :: String -> (Wire, Output)
readInit s = (Wire n, if v !! 2 == '1' then One else Zero) where
  (n, v) = span (/= ':') s

readGate :: String -> (Wire, Gate)
readGate s = (Wire res', gate) where
  (w1', r1) = span (/= ' ') s
  (op, r2) = span (/= ' ') (tail r1)
  (w2', r3) = span (/= ' ') (tail r2)
  res' = drop 4 r3
  gate | op == "AND" = Gate And (Wire w1') (Wire w2')
       | op == "OR" = Gate Or (Wire w1') (Wire w2')
       | op == "XOR" = Gate Xor (Wire w1') (Wire w2')
       | otherwise = undefined
