module Day9.Part2
(
  solve
) where

import AdventUtils
import Control.Applicative
import Data.Attoparsec.Text
import Data.Bifunctor
import Data.Char
import Data.Conduit
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Text as T

solve :: IO ()
solve =
  print . size =<<
    runConduitRes (
        textSource "input/day9/in.txt" =$= CA.sinkParser parser
    )

data Control = Unbounded | Bounded Int

decControl :: Int -> Control -> Control
decControl _ Unbounded   = Unbounded
decControl i (Bounded n) = Bounded (n - i)

isValid :: Control -> Bool
isValid (Bounded n) = n > 0
isValid Unbounded   = True

newtype CharCount = CC {
    getCharCount :: Int
  } deriving Show

data DecompTree = DT {
    charCount :: CharCount
  , repeats   :: [Repeat]
  }

data Repeat = Repeat Int DecompTree deriving Show

instance Show DecompTree where
  show (DT cc rs) = show cc ++ " " ++ show rs

incCharCount :: DecompTree -> DecompTree
incCharCount dt@(DT cc _) = dt { charCount = CC $ 1 + getCharCount cc }

addRepeat :: Repeat -> DecompTree -> DecompTree
addRepeat r dt@(DT _ rs) = dt { repeats = r : rs }

emptyDT :: DecompTree
emptyDT = DT (CC 0) []

size :: DecompTree -> Int
size (DT cc rs) = getCharCount cc + sum (map sizeOf rs)
  where sizeOf (Repeat n dt) = n * size dt

runParse :: T.Text -> Result DecompTree
runParse = parse parser

parser = parseDecompressionTree emptyDT Unbounded

matchWithLen :: Parser a -> Parser (Int, a)
matchWithLen = fmap (bimap T.length id) . match

parseDecompressionTree :: DecompTree -> Control -> Parser DecompTree
parseDecompressionTree dt ct
  | not $ isValid ct = return dt
  | otherwise        = (endOfInput >> return dt) <|> (anyChar >>= continueParse)
  where
    continueParse c
      | isSpace c = parseDecompressionTree dt ct
      | c == '('  = do
        (l, dt') <- parseDecompressionControl dt
        parseDecompressionTree dt' (decControl l ct)
      | otherwise = parseDecompressionTree (incCharCount dt) (decControl 1 ct)

parseDecompressionControl :: DecompTree -> Parser (Int, DecompTree)
parseDecompressionControl dt = do
  (nl, n) <- matchWithLen decimal
  char 'x'
  (rl, r) <- matchWithLen decimal
  char ')'
  let b = Bounded n
  (l, dt') <- matchWithLen (parseDecompressionTree emptyDT b)
  return (3 + nl + rl + l, addRepeat (Repeat r dt') dt)
