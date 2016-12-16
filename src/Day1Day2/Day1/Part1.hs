module Day1Day2.Day1.Part1
(
  solve
) where

import Day1Day2.Common
import Day1Day2.Day1.Utils
import Data.List
import System.Environment

solve :: IO ()
solve = print . taxicab origin . travel initial =<< getLine
  where
    origin  = (0, 0)
    initial = (U, origin)

travel :: MoveState -> String -> Coords
travel start =  snd                                    .
                foldl' (execTransition noBounds) start .
                getInstructions
  where noBounds = Nothing
