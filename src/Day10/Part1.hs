{-# LANGUAGE FlexibleContexts #-}

module Day10.Part1 where

import AdventUtils
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M hiding (Map)
import Data.Maybe

solve :: IO ()
solve = undefined

data Bot = Bot Int (Maybe Int) (Maybe Int)

newtype Bots = Bots { getBots :: Map Int Bot }

type Give = (Bot -> Maybe (Int, Bot))
type Accept = (Int -> Bot -> Bot)

newBot bid = Bot bid Nothing Nothing

low, high :: Give
low (Bot _ Nothing _)  = Nothing
low (Bot i (Just m) h) = Just (m, Bot i Nothing h)
high (Bot _ _ Nothing)  = Nothing
high (Bot i l (Just h)) = Just (h, Bot i l Nothing)

acceptLow, acceptHigh :: Accept
acceptLow l (Bot bid _ h) = Bot bid (Just l) h
acceptHigh h (Bot bid l _) = Bot bid l (Just h)

giveFromTo :: (MonadState Bots m) => Give -> Accept -> Int -> Int -> m ()
giveFromTo give accept b1 b2 = do
  mvAndB <- gets (give . fromJust . M.lookup b1 . getBots)
  let Just (v, bot1) = mvAndB
  when (isNothing mvAndB)
       (fail $ "Trying to take from nonexistent bot " ++ show b1)
  bot2   <- accept v <$> gets (M.findWithDefault (newBot b2) b2 . getBots)
  modify' (Bots . M.insert b1 bot1 . getBots)
  modify' (Bots . M.insert b2 bot2 . getBots)
