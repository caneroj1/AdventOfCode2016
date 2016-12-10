module Day1.Part1 where

import Day1.Utils
import Data.List
import System.Environment

solve :: IO ()
solve = print . taxicab origin . travel initial =<< getLine
  where
    origin  = (0, 0)
    initial = (N, origin)

travel :: (Dir, Coords) -> String -> Coords
travel start =  snd . foldl' execInstruction start .
                map read                           .
                words                              .
                filter (/= ',')
