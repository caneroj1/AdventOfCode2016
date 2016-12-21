module Day6.Utils
(
  solveViaMax
, solveViaMin
) where

import AdventUtils
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Resource
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as M
import Data.List
import Data.Ord
import Data.Text (unpack, Text)
import Data.Vector (Vector)
import qualified Data.Vector as V hiding (Vector)

type CharMap = M.HashMap Char Int

newtype CountPerCol = CPC {
    countPerCol :: Vector CharMap
  } deriving (Show)

type CountM = StateT CountPerCol IO

initial :: CountPerCol
initial = CPC $ V.replicate 8 M.empty

updateCountPerCol :: [(Int, Char)] -> CountPerCol -> CountPerCol
updateCountPerCol cs = CPC . flip (V.accum updateMap) cs . countPerCol
  where updateMap m c = M.insertWith (+) c 1 m

calcMessage f = V.foldr' (\m c -> chosenChar f m : c) [] . countPerCol

chosenChar f = fst . f (comparing snd) . M.toList

solveViaMax = solveWithMethod maximumBy
solveViaMin = solveWithMethod minimumBy

solveWithMethod method =
  print . calcMessage method =<< (flip execStateT initial .
            runConduitRes $
              textSourceL "input/day6/in.txt" =$= updateColumnCounts)

updateColumnCounts :: Consumer Text (ResourceT CountM) ()
updateColumnCounts = CC.mapM_ doUpdate
  where
    doUpdate :: Text -> ResourceT CountM ()
    doUpdate t =
      modify' (updateCountPerCol . zip [0..] $ unpack t)
