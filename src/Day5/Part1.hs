{-# LANGUAGE OverloadedStrings #-}

module Day5.Part1
(
  solve
) where

import qualified Data.ByteString as BS
import Data.Word
import Day5.Utils

solve =
  print (buildUpPassword nextPassword BS.reverse BS.empty doorID)

doorID = "abbhdwsy"

nextPassword :: PasswordFn
nextPassword digest password = Just $ BS.cons (nextComponent digest) password

nextComponent :: BS.ByteString -> Word8
nextComponent = flip BS.index 5
