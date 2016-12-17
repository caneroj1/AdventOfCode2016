{-# LANGUAGE OverloadedStrings #-}

module Day5.Part2 where

import Data.Bifunctor
import qualified Data.ByteString as BS
import Data.Char
import Data.Maybe
import Data.Word
import Day5.Utils
import Text.Read

solve =
  print (buildUpPassword nextPassword id (BS.replicate 8 0) doorID)

doorID = "abbhdwsy"

toPosition :: BS.ByteString -> Maybe Int
toPosition bs = (\n -> if n < 8 then Just n else Nothing)
  =<< (readMaybe . return . chr . fromIntegral $ BS.index bs 5)

nextComponent :: BS.ByteString -> Word8
nextComponent = flip BS.index 6

nextPassword :: PasswordFn
nextPassword digest password
  | isJust p  =
    if BS.index password i == 0
      then Just $ replaceAt i (nextComponent digest) password
      else Nothing
  | otherwise = Nothing
  where p = toPosition digest
        i = fromJust p

replaceAt :: Int -> Word8 -> BS.ByteString -> BS.ByteString
replaceAt i w = uncurry insertBetween . bimap id BS.tail . BS.splitAt i
  where insertBetween l r = l `BS.append` (w `BS.cons` r)
