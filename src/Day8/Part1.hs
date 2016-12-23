{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Day8.Part1 where

import Control.Lens hiding (at)
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Vector (Vector, (//))
import Data.Vector.Lens
import qualified Data.Vector as V hiding (Vector)

data Pixel = On | Off

instance Show Pixel where
  show On = "#"
  show Off = "."

data Action =
  Row Int Int |
  Col Int Int |
  Rect Int Int

newtype Matrix = M {
    fromMatrix :: Vector (Vector Pixel)
  }

instance Show Matrix where
  show (M vv) = intercalate "\n" . map show $ V.toList vv

at :: Int -> Lens' (Vector a) (Vector a)
at i = sliced i 1

trans :: Iso' (Vector (Vector a)) (Vector (Vector a))
trans = iso vl vl
  where lv = V.fromList . map V.fromList
        vl = lv . transpose . V.toList . V.map V.toList

shiftL, shiftR :: Int -> Vector a -> Vector a
shiftL i v = V.fromList . take (V.length v) . drop i . cycle $ V.toList v
shiftR i v = V.fromList . take l . drop (l - i) . cycle $ V.toList v
  where l = V.length v

matrix :: Iso' Matrix (Vector (Vector Pixel))
matrix = iso fromMatrix M

setRect :: Int -> Int -> Vector (Vector Pixel) -> Vector (Vector Pixel)
setRect r c vv = vv // zip [0..(r-1)] (repeat v')
  where v' = V.replicate l Off & sliced 0 c . mapped .~ On
        l  = V.length $ V.head vv

initial :: Int -> Int -> Matrix
initial r c = M $ V.replicate r (V.replicate c Off)

exec :: (MonadState Matrix m) => Action -> m ()
exec (Row i n)  = modify' $ over (matrix . at i) (shiftR n)
exec (Col i n)  = modify' $ over (matrix . trans . at i) (shiftR n)
exec (Rect c r) = modify' $ over matrix (setRect r c)
