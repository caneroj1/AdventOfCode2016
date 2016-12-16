{-# LANGUAGE TupleSections #-}

module Day1Day2.Day1.Utils where

import Day1Day2.Common

data Instr = LeftT Int | RightT Int

instance Read Instr where
  readsPrec _ ('L':ls) = return (LeftT $ read ls, [])
  readsPrec _ ('R':ls) = return (RightT $ read ls, [])
  readsPrec _ _        = []

instance Transition Instr where
  transition (LeftT _) e
    | e == maxBound = minBound
    | otherwise     = succ e
  transition (RightT _) e
    | e == minBound = maxBound
    | otherwise     = pred e

  magnitude (LeftT i) = i
  magnitude (RightT i) = i

getInstructions :: String -> [Instr]
getInstructions =
  map read                               .
  words                                  .
  filter (/= ',')

taxicab :: Coords -> Coords -> Int
taxicab (xa, ya) (xb, yb) = abs (xa - xb) + abs (ya - yb)

interp :: Int -> Int -> [Int]
interp x y
  | x > y     = reverse [y..x]
  | otherwise = [x..y]

-- invariant: l r have only one coordinate different b/w them.
-- either x or y values
allSteps :: Coords -> Coords -> [Coords]
allSteps l@(lx, ly) r@(rx, ry)
  | lx == rx = newCoords (lx,) ly ry
  | ly == ry = newCoords (,ly) lx rx
  where
    newCoords tf b = init . map tf . interp b
