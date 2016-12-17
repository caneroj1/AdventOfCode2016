{-# LANGUAGE OverloadedStrings #-}

module Day5.Utils
(
  PasswordFn
, buildUpPassword
) where

import Crypto.Hash.MD5
import Data.Hex
import qualified Data.ByteString as BS
import Data.Char

hexDigest :: BS.ByteString -> BS.ByteString
hexDigest = hex . hash

isInteresting :: BS.ByteString -> Bool
isInteresting bs = "00000" `BS.isPrefixOf` bs

conv :: Int -> BS.ByteString
conv = BS.pack . map (fromIntegral . ord) . show

type PasswordFn = (BS.ByteString -> BS.ByteString -> Maybe BS.ByteString)
type Finalizer = (BS.ByteString -> BS.ByteString)

buildUpPassword :: PasswordFn
                -> Finalizer
                -> BS.ByteString
                -> BS.ByteString
                -> BS.ByteString
buildUpPassword p f startingPassword doorID = go 0 0 startingPassword
  where
    go idx added password
      | added == 8       = f password
      | isInteresting hd =
        maybe (go idx' added password) (go idx' (added + 1)) $ p hd password
      | otherwise        = go idx' added password
      where curr      = doorID `BS.append` conv idx
            hd        = hexDigest curr
            idx'      = idx+1
