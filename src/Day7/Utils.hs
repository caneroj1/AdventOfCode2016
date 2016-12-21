{-# LANGUAGE GADTs #-}

module Day7.Utils
(
  IPV7(..)
, getIPV7s
) where

import AdventUtils
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V hiding (Vector)
import Text.Parsec

getIPV7s :: (MonadResource m) => Producer m (IPV7 Char)
getIPV7s =
      textSourceL "input/day7/in.txt" =$=
      CC.map parseIPV7Packet          =$=
      CL.catMaybes

data IPV7 a where
  V7 :: (Eq a, Show a) => [Vector a] -> [Vector a] -> IPV7 a

instance (Show a) => Show (IPV7 a) where
  show (V7 ss hs) = "V7 " ++ show ss ++ " " ++ show hs

empty = V7 [] []

bracketed :: Parsec Text u a -> Parsec Text u a
bracketed = between (char '[') (char ']')

parseS, parseH :: IPV7 Char -> Parsec Text () (IPV7 Char)
parseS v@(V7 ss hs) =
  option v $ do
    sv <- V.fromList <$> many1 letter
    parseH (V7 (sv : ss) hs)
parseH v@(V7 ss hs) =
  option v $ do
     hv <- V.fromList <$> bracketed (many1 letter)
     parseS (V7 ss (hv : hs))

parseIPV7Packet :: Text -> Maybe (IPV7 Char)
parseIPV7Packet t = either (const Nothing) return $ parse (parseS empty) "" t
