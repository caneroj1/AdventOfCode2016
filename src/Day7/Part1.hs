module Day7.Part1 where

import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Vector (Vector, (!))
import qualified Data.Vector as V hiding (Vector, (!))
import Day7.Utils

solve :: IO ()
solve =
  print =<<
    runConduitRes (
      getIPV7s                        =$=
      CC.map (fromEnum . supportsTLS) =$=
      CC.sum )

hasABBA :: (Eq a) => Vector a -> Bool
hasABBA v
  | V.length v < 4 = False
  | otherwise      =
    (v ! 0 == v ! 3 && v ! 1 == v ! 2 && v ! 0 /= v ! 1) || hasABBA (V.tail v)

supportsTLS :: IPV7 a -> Bool
supportsTLS (V7 sups hyps) = any hasABBA sups && not (any hasABBA hyps)
