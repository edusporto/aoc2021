module Day2.Parsing where

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
