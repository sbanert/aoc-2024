module Main where
import Data.Char (isDigit)

main :: IO ()
main = interact solve

solve :: String -> String
solve s = unlines ["Part 1: " ++ part1 s, "Part 2: " ++ part2 s]

part1 :: String -> String
part1 = show . sum . map (uncurry (*)) .  parseMul []

part2 :: String -> String
part2 = show . sum . map (uncurry (*)) . parseMulDo []

parseMulDo :: [(Int, Int)] -> String -> [(Int, Int)]
parseMulDo acc [] = acc
parseMulDo acc ('m':'u':'l':'(':r) = readFirstNumDo acc r
parseMulDo acc ('d':'o':'n':'\'':'t':'(':')':r) = waitForDo acc r
parseMulDo acc (_:t) = parseMulDo acc t

waitForDo :: [(Int, Int)] -> String -> [(Int, Int)]
waitForDo acc [] = acc
waitForDo acc ('d':'o':'(':')':r) = parseMulDo acc r
waitForDo acc (_:t) = waitForDo acc t

readFirstNumDo :: [(Int, Int)] -> String -> [(Int, Int)]
readFirstNumDo acc s = ans where
  (n', r) = span isDigit s
  ans | null n' || null r  = parseMulDo acc r
      | head r == ',' = readSecondNumDo (read n') acc (tail r)
      | otherwise = parseMulDo acc r

readSecondNumDo :: Int -> [(Int, Int)] -> String -> [(Int, Int)]
readSecondNumDo n acc s = ans where
  (m', r) = span isDigit s
  ans | null m' || null r = parseMulDo acc r
      | head r == ')' = parseMulDo ((n, read m'):acc) (tail r)
      | otherwise = parseMulDo acc r

----- Part 1 functions ------

parseMul :: [(Int, Int)] -> String -> [(Int, Int)]
parseMul acc [] = acc
parseMul acc ('m':'u':'l':'(':r) = readFirstNum acc r
parseMul acc (_:t) = parseMul acc t

readFirstNum :: [(Int, Int)] -> String -> [(Int, Int)]
readFirstNum acc s = ans where
  (n', r) = span isDigit s
  ans | null n' || null r  = parseMul acc r
      | head r == ',' = readSecondNum (read n') acc (tail r)
      | otherwise = parseMul acc r

readSecondNum :: Int -> [(Int, Int)] -> String -> [(Int, Int)]
readSecondNum n acc s = ans where
  (m', r) = span isDigit s
  ans | null m' || null r = parseMul acc r
      | head r == ')' = parseMul ((n, read m'):acc) (tail r)
      | otherwise = parseMul acc r
