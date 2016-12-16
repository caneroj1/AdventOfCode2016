module Day1Day2.Day2.Part2
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
bounds = Just $ Fn check
  where
    check (0, 0) = False
    check (1, 0) = False
    check (0, 1) = False

    check (0, 3) = False
    check (1, 4) = False
    check (0, 4) = False

    check (3, 0) = False
    check (4, 0) = False
    check (4, 1) = False

    check (3, 4) = False
    check (4, 4) = False
    check (4, 3) = False

    check (x, y) = 0 <= x && x <= 4 && 0 <= y && y <= 4

initial :: MoveState
initial = (U, (0, 2))

buttons =
  [
    [undefined, undefined, "5", undefined, undefined]
  , [undefined, "A"      , "6", "2"      , undefined]
  , ["D"      , "B"      , "7", "3"      , "1"      ]
  , [undefined, "C"      , "8", "4"      , undefined]
  , [undefined, undefined, "9", undefined, undefined]
  ]
