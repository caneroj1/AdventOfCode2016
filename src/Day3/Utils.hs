module Day3.Utils
(
  validateTriangle
) where

import Data.Text (Text)
import qualified Data.Text as T hiding (Text)
import Data.Text.Read

data Triangle = Triangle Int Int Int deriving Show

isValid :: Triangle -> Bool
isValid (Triangle a b c) = (a + b > c) &&
                           (a + c > b) &&
                           (b + c > a)

lineToTri :: Text -> Either String Triangle
lineToTri = toTriangle . take 3 . T.words
  where
    toTriangle [a,b,c] = do
      (a', _) <- decimal a
      (b', _) <- decimal b
      (c', _) <- decimal c
      return $ Triangle a' b' c'

validateTriangle :: Text -> Int
validateTriangle = either (const 0) (fromEnum . isValid) . lineToTri
