{-# LANGUAGE OverloadedStrings #-}

module Day9.Part1 where

import AdventUtils
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Char
import qualified Data.Conduit.Combinators as CC
import Data.Text (Text)
import qualified Data.Text as T hiding (Text)
import Data.Text.Read
import Data.Maybe

data Decompress = Decompress Int Int

solve :: IO ()
solve =
  print =<< runConduitRes (
    textSource "input/day9/in.txt" =$= control =$= CC.length
  )

mkDecompress :: Text -> Maybe Decompress
mkDecompress = either (const Nothing) Just . getInts . T.splitOn "x"
  where
    getInts [x, y] = do
      (i1, _) <- decimal x
      (i2, _) <- decimal y
      return $ Decompress i1 i2

isMarker :: Char -> Bool
isMarker c = '(' == c

isEndMarker :: Char -> Bool
isEndMarker c = ')' == c

control :: (MonadResource m) => Conduit Text m Char
control = do
  c <- CC.headE
  when (isJust c) (handleChar (fromJust c) >> control)

consumeAndDecompress :: (MonadResource m)
                     => Text
                     -> Conduit Text m Char
consumeAndDecompress t = do
  c <- CC.headE
  consume c
  where consume Nothing  = return ()
        consume (Just c)
          | isEndMarker c = decompress $! mkDecompress t
          | otherwise     = consumeAndDecompress (T.snoc t c)
        decompress Nothing                 = return ()
        decompress (Just (Decompress n r)) = consumeNR n r T.empty

consumeNR :: (MonadResource m) => Int -> Int -> Text -> Conduit Text m Char
consumeNR 0 r t = CC.yieldMany (T.replicate r t)
consumeNR n r t = do
  c <- CC.headE
  when (isJust c) (consumeNR (n-1) r (t `T.snoc` fromJust c))

handleChar :: (MonadResource m) => Char -> Conduit Text m Char
handleChar c
  | isMarker c = consumeAndDecompress T.empty
  | otherwise  = unless (isSpace c) (yield c) >> control
