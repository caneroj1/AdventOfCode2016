{-# LANGUAGE Rank2Types #-}

module Day8.Utils
(
  forEachCmd
) where

import AdventUtils
import Control.Monad.Morph
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Text (Text)
import Day8.Parser
import Day8.Types

type Action = Cmd -> State Matrix ()
type MatrixM = StateT Matrix IO

getAction :: (MonadResource m) => Conduit Text m Parsed
getAction = CC.map parseAction

runAction :: Action -> Consumer Parsed (ResourceT MatrixM) ()
runAction a = CC.mapM_ (handleParsed a)
  where handleParsed _ (Left e)  = fail $ show e
        handleParsed f (Right v) = lift . hoist generalize $ f v

forEachCmd :: Int -> Int -> Action -> IO Matrix
forEachCmd r c f =
  flip execStateT s $
    runConduitRes (
      textSourceL "input/day8/in.txt" =$=
      getAction                       =$=
      runAction f
    )
  where s = initial r c
