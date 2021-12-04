module Day4.Parsing where

import Control.Monad (liftM)

split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
  "" -> []
  s' -> w : split p s''
    where
      (w, s'') = break p s'

keyValue :: String -> (String, String)
keyValue str = (head s, s !! 1)
  where
    s = split (' ' ==) str



-- https://www.py4u.net/discuss/1985711

-- this operates purely on list of strings
-- and also will fail horribly when passed something that doesn't
-- match the pattern
parseLines :: [String] -> (Int, Int, [Int], [[Int]])
parseLines (mn_line : ks_line : matrix_lines) = (m, n, ks, matrix)
  where
    [m, n] = readInts mn_line
    ks = readInts ks_line
    matrix = parseMatrix matrix_lines

-- this here is to loop through remaining lines to form a matrix
parseMatrix :: [String] -> [[Int]]
parseMatrix lines = parse_matrix' lines []
  where
    parse_matrix' [] acc = reverse acc
    parse_matrix' (l : ls) acc = parse_matrix' ls $ readInts l : acc

-- this here is to give proper signature for read
readInts :: String -> [Int]
readInts = map read . words

-- this reads the file contents and lifts the result into IO
parseFile :: FilePath -> IO (Int, Int, [Int], [[Int]])
parseFile filename = do
  file_lines <- (fmap lines . readFile) filename
  return $ parseLines file_lines

-- https://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative or zero n"
