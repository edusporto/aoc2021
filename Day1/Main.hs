module Day1.Main where

part1 :: [Int] -> Int
part1 (x1 : x2 : xs)
  | x1 < x2 = 1 + part1 (x2 : xs) -- increased
  | otherwise = part1 (x2 : xs)
part1 _ = 0

part2 :: [Int] -> Int
part2 (x1 : x2 : x3 : x4 : xs)
  | a < b = 1 + part2 rest -- increased
  | otherwise = part2 rest
  where
    a = x1 + x2 + x3
    b = x2 + x3 + x4
    rest = x2 : x3 : x4 : xs
part2 _ = 0

main :: IO ()
main = do
  text <- readFile "Day1/input.txt"
  let numbers = map read (lines text)

  print $ part1 numbers
  print $ part2 numbers
