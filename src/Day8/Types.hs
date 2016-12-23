module Day8.Types where

import Data.Vector (Vector, (//))
import qualified Data.Vector as V hiding (Vector, (//))

data Pixel = Off | On deriving Enum

instance Show Pixel where
  show On = "#"
  show Off = "."

data Cmd =
  Row Int Int |
  Col Int Int |
  Rect Int Int

newtype Matrix = M {
    fromMatrix :: Vector (Vector Pixel)
  }

instance Show Matrix where
  show (M vv) = unlines . map (unwords . map show . V.toList) $ V.toList vv

isLit :: Pixel -> Bool
isLit = toEnum . fromEnum

lit :: Matrix -> Int
lit = V.foldl' (\s v -> s + V.length (V.filter isLit v)) 0 . fromMatrix

initial :: Int -> Int -> Matrix
initial r c = M $ V.replicate r (V.replicate c Off)
