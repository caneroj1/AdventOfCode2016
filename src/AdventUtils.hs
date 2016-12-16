{-# LANGUAGE Rank2Types #-}

module AdventUtils where

import Control.Monad.Trans.Resource
import Data.Bifunctor
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import Data.Text (Text)

textSource :: (MonadResource m) => FilePath -> Producer m Text
textSource p = CB.sourceFile p =$= CT.decodeUtf8

textSourceL :: (MonadResource m) => FilePath -> Producer m Text
textSourceL p = textSource p =$= CT.lines

unsnoc :: [a] -> Maybe ([a], a)
unsnoc []     = Nothing
unsnoc [a]    = Just ([], a)
unsnoc (a:as) = bimap (a :) id <$> unsnoc as
