module Day4.Part1
(
  solve
) where

import AdventUtils
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Day4.Utils

solve :: IO ()
solve =
  print =<<
    runConduitRes (textSourceL filename =$=
                   tryGetRooms          =$=
                   CC.foldl trySolveProblem 0)

filename = "input/day4/in.txt"

trySolveProblem :: Int -> Maybe Problem -> Int
trySolveProblem i Nothing  = i
trySolveProblem i (Just p)
  | isRealRoom p = i + sectorID (room p)
  | otherwise = i
