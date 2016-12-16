module Day1Day2.Day1.Part2
(
  solve
) where

import Day1Day2.Common
import Day1Day2.Day1.Utils
import System.Environment
import Data.Set (Set)
import qualified Data.Set as S hiding (Set)

solve :: IO ()
solve = print . fmap (taxicab origin) . findFirstVisited initial =<< getLine
  where
    origin  = (0, 0)
    initial = (U, origin)

findFirstVisited :: MoveState -> String -> Maybe Coords
findFirstVisited start =  findFirstDup                          .
                          map snd                               .
                          scanl (execTransition noBounds) start .
                          getInstructions
  where noBounds = Nothing

findFirstDup :: [Coords] -> Maybe Coords
findFirstDup = check S.empty . toSteps
  where
    toSteps []       = []
    toSteps [x]      = [x]
    toSteps (x:y:xs) = allSteps x y ++ toSteps (y:xs)
    check st []     = Nothing
    check st (x:xs)
      | S.member x st = Just x
      | otherwise     = check (S.insert x st) xs
