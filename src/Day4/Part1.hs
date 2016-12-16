{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day4.Part1
(
  solve
) where

import AdventUtils
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Bifunctor
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import qualified Data.HashMap.Strict as M
import Data.Either
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T hiding (Text)
import Text.Parsec

solve :: IO ()
solve = undefined
  -- print =<<
  --   runConduitRes (textSourceL "input/day4/in.txt" =$= sumOfRealTriangles)

type SectorID = Int
type Checksum = String
type Counts = M.HashMap Char Int

data Room = Room {
      sectorID :: SectorID
    , checksum :: Checksum
  }

type Problem = (Counts, Room)

filename = "input/day4/in.txt"

getIDAndChecksum :: Text -> Maybe Room
getIDAndChecksum = either (const Nothing) Just . parse parseIDChecksum filename
  where
    parseIDChecksum = do
      sectorID <- read <$> many1 digit
      checksum <- between (char '[') (char ']') (count 5 letter)
      return $ Room sectorID checksum

solution :: Consumer Text (ResourceT IO) Int
solution = CC.map tryGetProblem       =$=
           CC.foldl trySolveProblem 0
            -- CC.filter isJust                =$=
            -- CC.map
  where
    -- isIDandChecksum =
    -- tryGetRoom t = ap fmap roomBimap . unsnoc . T.splitOn "-"
    maybeMkRoom (ts, t) = (letterCounts ts, ) <$> getIDAndChecksum t
    tryGetProblem t     = maybeMkRoom =<< unsnoc (T.splitOn "-" t)

letterCounts :: [Text] -> Counts
letterCounts = T.foldl' count M.empty . T.concat
  where count m c = M.insertWith (+) c 1 m

trySolveProblem :: Int -> Maybe Problem -> Int
trySolveProblem i Nothing          = i
trySolveProblem i (Just (cts, rm))
  | mostFrequentLetters cts == checksum rm = i + sectorID rm
  | otherwise = i

mostFrequentLetters :: Counts -> String
mostFrequentLetters = take 5 . map fst . sortBy cmp . M.toList
  where
    cmp (k1, v1) (k2, v2) = v1 `compare` v2 <> k1 `compare` k2
--
-- sumOfRealTriangles :: Consumer Text (ResourceT IO) Int
-- sumOfRealTriangles = CC.foldl triFold 0
--   where
--     triFold acc = (+) acc . validateTriangle
