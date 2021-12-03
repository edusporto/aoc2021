module Day2.Main where

import Day2.Parsing (keyValue)
import Control.Applicative (liftA2)

data Direction
  = Forward Int
  | Down Int
  | Up Int

data Position = Position
  { x :: Int,
    y :: Int,
    aim :: Int
  }
  deriving (Show)

move1 :: Position -> Direction -> Position
move1 (Position x y aim) dir =
  case dir of
    Forward dist -> Position (x + dist) y aim
    Down dist    -> Position x (y + dist) aim
    Up dist      -> Position x (y - dist) aim

part1 :: [Direction] -> Position
part1 = foldl move1 (Position 0 0 0)

move2 :: Position -> Direction -> Position
move2 (Position x y aim) dir =
  case dir of
    Forward dist -> Position (x + dist) (y + aim * dist) aim
    Down dist    -> Position x y (aim + dist)
    Up dist      -> Position x y (aim - dist)

part2 :: [Direction] -> Position
part2 = foldl move2 (Position 0 0 0)

-- main

main :: IO ()
main = do
  text <- readFile "Day2/input.txt"
  let directions = map (convert . keyValue) (lines text)

  let p1 = part1 directions
  let p2 = part2 directions

  print p1
  print $ liftA2 (*) x y p1
  print p2
  print $ liftA2 (*) x y p2

-- Parsing

convert :: (String, String) -> Direction
convert ("forward", val) = Forward (read val)
convert ("down", val) = Down (read val)
convert ("up", val) = Up (read val)
convert (str, val) = error $ str ++ " shouldn't happen"
