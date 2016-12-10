module Day1.Part2 where

import Day1.Utils
import System.Environment
import Data.Set (Set)
import qualified Data.Set as S hiding (Set)

solve :: IO ()
solve = print . fmap (taxicab origin) . findFirstVisited initial =<< getLine
  where
    origin  = (0, 0)
    initial = (N, origin)

findFirstVisited :: (Dir, Coords) -> String -> Maybe Coords
findFirstVisited start =  findFirstDup                .
                          map snd                     .
                          scanl execInstruction start .
                          map read                    .
                          words                       .
                          filter (/= ',')

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
