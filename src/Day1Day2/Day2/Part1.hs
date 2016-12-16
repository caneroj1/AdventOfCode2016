module Day1Day2.Day2.Part1
(
  solve
) where

import Day1Day2.Common
import Day1Day2.Day2.Utils
import Data.List
import System.Environment
import System.IO

solve =
  mapM_ putStrLn =<<
  (getButtons buttons bounds initial . getInstructions) <$>
    (hGetContents =<< openFile "input/day2/in.txt" ReadMode)

bounds :: Maybe Bounds
bounds = Just $ Pts ((0, 0), (2, 2))

initial :: MoveState
initial = (U, (1, 1))

buttons = [["7", "4", "1"], ["8", "5", "2"], ["9", "6", "3"]]
