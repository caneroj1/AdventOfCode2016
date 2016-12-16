module Day1Day2.Common
(
  Transition(..)
, Coords
, Bounds(..)
, Dir(..)
, MoveState
, execTransition
) where

import Data.Bifunctor

data Dir = U | R | D | L deriving (Enum, Show, Bounded, Eq, Read)

type Coords = (Int, Int)
data Bounds = Pts (Coords, Coords) | Fn (Coords -> Bool)

class Transition a where
  transition :: a -> Dir -> Dir
  magnitude :: a -> Int

snap :: Coords -> Coords -> Bounds -> Coords
snap _ (x, y) (Pts ((minX, minY), (maxX, maxY))) =
  (min maxX $ max minX x , min maxY $ max minY y)
snap old new (Fn f) = if f new then new else old

progress :: Dir -> Coords -> Int -> Coords
progress U c i = bimap id           (+i)         c
progress R c i = bimap (+i)         id           c
progress D c i = bimap id           (subtract i) c
progress L c i = bimap (subtract i) id           c

type MoveState = (Dir, Coords)

execTransition :: (Transition a) => Maybe Bounds -> MoveState -> a -> MoveState
execTransition mb (curr, coords) t =
  let next    = transition t curr
      coords' = progress next coords $ magnitude t
    in (next, maybe coords' (snap coords coords') mb)
