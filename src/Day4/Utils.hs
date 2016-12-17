{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day4.Utils
(
  tryGetRooms
, isRealRoom
, Room(..)
, Problem(..)
) where

import AdventUtils
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import qualified Data.HashMap.Strict as M
import Data.Either
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T hiding (Text)
import Text.Parsec

type SectorID = Int
type Checksum = String
type Counts = M.HashMap Char Int

data Room = Room {
      sectorID :: SectorID
    , checksum :: Checksum
  } deriving Show

data Problem = P {
    name   :: [Text]
  , counts :: Counts
  , room   :: Room
  }

tryGetRooms :: Conduit Text (ResourceT IO) (Maybe Problem)
tryGetRooms = CC.map tryGetProblem
  where
    maybeMkRoom (ts, t) = P ts (letterCounts ts) <$> getIDAndChecksum t
    tryGetProblem t     = maybeMkRoom =<< unsnoc (T.splitOn "-" t)

isRealRoom :: Problem -> Bool
isRealRoom (P _ cts rm) = mostFrequentLetters cts == checksum rm

getIDAndChecksum :: Text -> Maybe Room
getIDAndChecksum = either (const Nothing) Just . parse parseIDChecksum ""
  where
    parseIDChecksum = do
      sectorID <- read <$> many1 digit
      checksum <- between (char '[') (char ']') (count 5 letter)
      return $ Room sectorID checksum

letterCounts :: [Text] -> Counts
letterCounts = T.foldl' count M.empty . T.concat
  where count m c = M.insertWith (+) c 1 m

mostFrequentLetters :: Counts -> String
mostFrequentLetters = take 5 . map fst . sortBy cmp . M.toList
  where
    cmp (k1, v1) (k2, v2) = Down v1 `compare` Down v2 <> k1 `compare` k2
