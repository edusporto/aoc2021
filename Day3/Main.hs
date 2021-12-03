module Day3.Main where

import Control.Applicative (liftA2)
import Data.Bits (complement)
import Data.List (sortBy)
import Data.Map (Map, fromListWith, toList)

frequencyTable :: (Ord k) => [k] -> Map k Int
frequencyTable list = fromListWith (+) [(key, 1) | key <- list]

compareBySnd :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
compareBySnd x y =
  case snd x `compare` snd y of
    LT -> LT
    EQ -> fst x `compare` fst y
    GT -> GT

sortByFrequency :: Ord b => [b] -> [b]
sortByFrequency list = map fst $ sortBy compareBySnd (toList (frequencyTable list))

mostFrequent :: Ord a => [a] -> a
mostFrequent list = last (sortByFrequency list)

leastFrequent :: Ord a => [a] -> a
leastFrequent list = head (sortByFrequency list)

transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose xs = map head xs : transpose (map tail xs)

binaryToInt :: String -> Int
binaryToInt bin = binaryToInt' 0 (reverse bin)

charToBinary :: Char -> Int
charToBinary '0' = 0
charToBinary '1' = 1
charToBinary _ = error "not binary"

binaryToInt' :: Int -> String -> Int
binaryToInt' power [] = 0
binaryToInt' power (digit : digits) =
  charToBinary digit * 2 ^ power
    + binaryToInt' (power + 1) digits

gamma :: [String] -> Int
gamma numbers = binaryToInt $ map mostFrequent (transpose numbers)

epsilon :: [String] -> Int
epsilon numbers = binaryToInt $ map leastFrequent (transpose numbers)

part1 :: [String] -> Int
part1 = liftA2 (*) gamma epsilon

oxygenRating :: [String] -> Int
oxygenRating numbers = binaryToInt $ calcRating mostFrequent numbers 0

co2Rating :: [String] -> Int
co2Rating numbers = binaryToInt $ calcRating leastFrequent numbers 0

calcRating :: Ord a => ([a] -> a) -> [[a]] -> Int -> [a]
calcRating _ [] _ = error "no numbers left for rating"
calcRating _ [number] _ = number
calcRating whichFilter numbers index = calcRating whichFilter filtered (index + 1)
  where
    filters = map whichFilter (transpose numbers)
    currFilter = filters !! index
    ith = map (!! index) numbers
    filtered = [ns | (n, ns) <- zip ith numbers, n == currFilter]

part2 :: [String] -> Int
part2 = liftA2 (*) oxygenRating co2Rating

main :: IO ()
main = do
  text <- readFile "Day3/input.txt"

  print $ part1 (lines text)
  print $ part2 (lines text)
