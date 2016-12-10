{-# LANGUAGE TupleSections #-}

module Day1.Utils where

import Data.Bifunctor

class (Enum a, Bounded a, Eq a) => Wrapped a where
  succWrap :: a -> a
  succWrap e
    | e == maxBound = minBound
    | otherwise     = succ e
  predWrap :: a -> a
  predWrap e
    | e == minBound = maxBound
    | otherwise     = pred e

class Cmd a where
  cmd :: (Wrapped b) => a -> (b -> b)

data Dir = N | E | S | W deriving (Enum, Show, Bounded, Eq)

instance Wrapped Dir

data Instr = L Int | R Int

instance Read Instr where
  readsPrec _ ('L':ls) = return (L $ read ls, [])
  readsPrec _ ('R':ls) = return (R $ read ls, [])
  readsPrec _ _        = []

magnitude :: Instr -> Int
magnitude (L i) = i
magnitude (R i) = i

turnLeft, turnRight :: Wrapped a => a -> a
turnLeft = predWrap
turnRight = succWrap

instance Cmd Instr where
  cmd (L _) = turnLeft
  cmd (R _) = turnRight

type Coords = (Int, Int)

updateCoords :: Dir -> Int -> Coords -> Coords
updateCoords N i = bimap id           (+i)
updateCoords E i = bimap (+i)         id
updateCoords S i = bimap id           (subtract i)
updateCoords W i = bimap (subtract i) id

execInstruction :: (Dir, Coords) -> Instr -> (Dir, Coords)
execInstruction (curr, coords) instr =
  let next = cmd instr curr
      mag  = magnitude instr
    in (next, updateCoords next mag coords)

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
