{-# LANGUAGE OverloadedStrings #-}

module Day4.Part2
(
  solve
) where

import AdventUtils
import Control.Monad
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import Data.Char
import qualified Data.Text as T
import Day4.Utils

solve :: IO ()
solve =
    runConduitRes (textSourceL filename =$=
                   tryGetRooms          =$=
                   CL.catMaybes         =$=
                   CC.map decodeName    =$=
                   CC.print)
  where realRooms = maybe False isRealRoom

filename = "input/day4/in.txt"

decodeName :: Problem -> T.Text
decodeName (P orig _ rm) =
  T.append sText . T.intercalate " " $ [T.map (slideCharBy sID)] `ap` orig
  where sID = sectorID rm
        sText = T.pack $ show sID ++ " - "

slideCharBy :: Int -> Char -> Char
slideCharBy n c = chr $
  ord 'a' + ((ord c - ord 'a') + addFactor) `mod` 26
  where addFactor = n `mod` 26
