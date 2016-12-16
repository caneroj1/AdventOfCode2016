module Day1Day2.Day2.Utils where

import Control.Monad
import Data.List
import Day1Day2.Common

instance Transition Dir where
  transition new _ = new
  magnitude _     = 1

type Buttons = [[String]]

getInstructions :: String -> [[Dir]]
getInstructions = ap [map (read . return)] . lines

getButton :: Buttons -> Coords -> String
getButton buttons (x, y) = buttons !! x !! y

getButtons :: Buttons -> Maybe Bounds -> MoveState -> [[Dir]] -> [String]
getButtons _ _ ms []            = []
getButtons btns bds ms (ds:dss) =
  let ms'@(_, c) = foldl' (execTransition bds) ms ds
    in getButton btns c : getButtons btns bds ms' dss
