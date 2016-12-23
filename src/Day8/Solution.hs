{-# LANGUAGE FlexibleContexts #-}

module Day8.Solution
(
  solve
) where

import Control.Lens
import Control.Monad.State
import Data.Vector (Vector)
import Data.Vector.Lens
import qualified Data.Vector as V hiding (Vector)
import Day8.Lens
import Day8.Types
import Day8.Utils

solve :: IO ()
solve = (\m -> print m >> print (lit m)) =<< forEachCmd 6 50 exec

exec :: (MonadState Matrix m) => Cmd -> m ()
exec (Row i n)  = modify' $ matrix . ix i %~ shiftR n
exec (Col i n)  = modify' $ matrix . trans . ix i %~ shiftR n
exec (Rect c r) = modify' $ matrix %~ setRect r c

shiftR :: Int -> Vector a -> Vector a
shiftR i v = V.fromList . take l . drop (l - i) . cycle $ V.toList v
  where l = V.length v

setRect :: Int -> Int -> Vector (Vector Pixel) -> Vector (Vector Pixel)
setRect r c = V.imap turnOnRow
  where turnOnRow idx v
          | idx < r   = v & sliced 0 c . mapped .~ On
          | otherwise = v
