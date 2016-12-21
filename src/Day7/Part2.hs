module Day7.Part2 where

import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Hashable
import Data.HashSet (HashSet)
import qualified Data.HashSet as S hiding (HashSet)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V hiding (Vector, (!))
import Day7.Utils

solve :: IO ()
solve =
  print =<<
    runConduitRes (
      getIPV7s                        =$=
      CC.map (fromEnum . supportsSSL) =$=
      CC.sum )

type BABLookup a = HashSet [a]

supportsSSL :: (Hashable a) => IPV7 a -> Bool
supportsSSL v@(V7 sups hyps) = any (hasABAAndBAB lookupSet) sups
  where
    lookupSet = makeLookup hyps

hasABAAndBAB :: (Hashable a, Eq a) => BABLookup a -> Vector a -> Bool
hasABAAndBAB l v
  | V.length v < 3 = False
  | isABA s        = S.member b l || rest
  | otherwise      = rest
  where s    = sliceOfThree v
        b    = getBAB s
        rest = hasABAAndBAB l (sliceToEnd v)

sliceToEnd :: Vector a -> Vector a
sliceToEnd v = V.slice 1 (V.length v - 1) v

sliceOfThree :: Vector a -> Vector a
sliceOfThree = V.slice 0 3

isABA :: (Eq a) => Vector a -> Bool
isABA v = V.head v == V.last v

getBAB :: (Eq a) => Vector a -> [a]
getBAB v = [v ! 1, v ! 0, v ! 1]

makeLookup :: (Hashable a, Eq a) => [Vector a] -> BABLookup a
makeLookup = S.unions . map (fromVector S.empty)
  where
    fromVector m v
      | V.length v < 3 = m
      | otherwise      = fromVector (s `S.insert` m) (sliceToEnd v)
      where s = V.toList $ sliceOfThree v
