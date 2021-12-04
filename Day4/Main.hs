module Day4.Main where

import Data.List (find, (\\))
import Data.Map (Map, insertWith, toList, empty)
import Day4.Parsing (split, group, parseMatrix)

type Index2D = (Int, Int)

type IndexedMatrix a = [[(Index2D, a)]]

bingoSize :: Int
bingoSize = 5

indexedMatrix :: [[a]] -> IndexedMatrix a
indexedMatrix = indexedMatrix' 0

indexedMatrix' :: Int -> [[a]] -> IndexedMatrix a
indexedMatrix' _ [] = []
indexedMatrix' i (line : lines) =
  zip indices line : indexedMatrix' (i + 1) lines
  where
    indices = zip (repeat i) [0 ..]

data MatrixComponent
  = Line Int
  | Column Int
  | Diagonal Int
  deriving (Eq, Ord, Show)

data Bingo = Bingo
  { matrix :: IndexedMatrix Int,
    components :: Map MatrixComponent Int,
    numbersFound :: [Int]
  }
  deriving (Eq, Show)

-- could be done with better complexity if Bingo stored a set of its elements
findIndexedMatrix :: Eq a => IndexedMatrix a -> a -> Maybe Index2D
findIndexedMatrix [] _ = Nothing
findIndexedMatrix (line : lines) value =
  case find pred line of
    Just (index, _) -> Just index
    Nothing         -> findIndexedMatrix lines value
  where
    pred (_, currValue) = currValue == value

componentsIndex2D :: Int -> Int -> Index2D -> [MatrixComponent]
componentsIndex2D m n index =
  diagonalsIndex2D m n index ++ linesIndex2D m n index ++ columnsIndex2D m n index

diagonalsIndex2D :: Int -> Int -> Index2D -> [MatrixComponent]
diagonalsIndex2D m n (i, j)
  | (i == j) && (i == abs (j - n + 1)) = [Diagonal 0, Diagonal 1]
  | i == j                             = [Diagonal 0]
  | i == abs (j - n + 1)               = [Diagonal 1]
  | otherwise                          = []

linesIndex2D :: Int -> Int -> Index2D -> [MatrixComponent]
linesIndex2D m n (i, j) = [Line i]

columnsIndex2D :: Int -> Int -> Index2D -> [MatrixComponent]
columnsIndex2D m n (i, j) = [Column j]

updateComponents :: [MatrixComponent] -> Map MatrixComponent Int -> Map MatrixComponent Int
updateComponents components mapping =
  foldl
    (\mapping component -> insertWith (\_ old -> old - 1) component (bingoSize - 1) mapping)
    mapping
    components

bingoStep :: Int -> Bingo -> Bingo
bingoStep number (Bingo matrix components numbersFound) =
  case valueFound of
    Nothing    -> Bingo matrix components numbersFound
    Just index ->
      Bingo
        matrix
        (updateComponents (newComponents index) components)
        (number : numbersFound)
  where
    valueFound = findIndexedMatrix matrix number
    newComponents = componentsIndex2D bingoSize bingoSize

won :: Bingo -> Bool
won (Bingo matrix components _)
  | 0 `elem` map snd (toList components) = True
  | otherwise                            = False

winner :: [Bingo] -> [Int] -> (Bingo, Int)
winner bingos [] = error ("no winners " ++ show bingos)
winner bingos (number : numbers)
  | null filtered = winner newBingos numbers
  | otherwise     = (head filtered, number) -- (winner, winningNumber)
  where
    newBingos = map (bingoStep number) bingos
    filtered = filter won newBingos

createBingo :: [[Int]] -> Bingo
createBingo matrix = Bingo (indexedMatrix matrix) empty []

part1 :: [Bingo] -> [Int] -> Int
part1 bingos numbers = sum losingNumbers * winningNumber
  where
    (Bingo matrix _ winningNumbers, winningNumber) = winner bingos numbers
    allNumbers = map snd (concat matrix)
    losingNumbers = allNumbers \\ winningNumbers

lastWinner :: [Bingo] -> [Int] -> (Bingo, Int)
lastWinner bingos [] = error ("couldn't find last winner " ++ show bingos)
lastWinner bingos (number : numbers)
  | null filtered = lastWinner newBingos numbers
  | otherwise =
    if length bingos == 1
      then (head filtered, number) -- (winner, winningNumber)
      else lastWinner (newBingos \\ filtered) numbers
  where
    newBingos = map (bingoStep number) bingos
    filtered = filter won newBingos

part2 :: [Bingo] -> [Int] -> Int
part2 bingos numbers = sum losingNumbers * winningNumber
  where
    (Bingo matrix _ winningNumbers, winningNumber) = lastWinner bingos numbers
    allNumbers = map snd (concat matrix)
    losingNumbers = allNumbers \\ winningNumbers

main :: IO ()
main = do
  text <- readFile "Day4/input.txt"
  let ls = lines text
  let strMatrices = filter (/= "") ((tail . tail) ls)

  let numbers = read (head ls) :: [Int]
  let groups = group bingoSize strMatrices
  let matrices = map parseMatrix groups
  let bingos = map createBingo matrices

  print $ part1 bingos numbers
  print $ part2 bingos numbers
