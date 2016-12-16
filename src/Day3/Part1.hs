module Day3.Part1
(
  solve
) where

import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import Data.Text (Text)
import Day3.Utils

solve :: IO ()
solve =
  print =<< runConduitRes (source =$= sumOfRealTriangles)

source :: Producer (ResourceT IO) Text
source = CB.sourceFile "input/day3/in.txt" =$= CB.lines =$= CT.decodeUtf8

sumOfRealTriangles :: Consumer Text (ResourceT IO) Int
sumOfRealTriangles = CC.foldl triFold 0
  where
    triFold acc = (+) acc . validateTriangle
