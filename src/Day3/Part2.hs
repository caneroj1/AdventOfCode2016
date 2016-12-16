module Day3.Part2
(
  solve
) where

import AdventUtils
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T hiding (Text)
import Day3.Utils

solve :: IO ()
solve =
  print =<<
    runConduitRes (textSourceL file =$= sendPacket =$= handleTrianglePacket)
  where file = "input/day3/in.txt"

data Packet = Packet Text Text Text

sendPacket :: Conduit Text (ResourceT IO) (Maybe Packet)
sendPacket = do
  a <- await
  b <- await
  c <- await
  let mbPacket = Packet <$> a <*>
                            b <*>
                            c
  when (isJust mbPacket) (yield mbPacket >> sendPacket)

handleTrianglePacket :: Consumer (Maybe Packet) (ResourceT IO) Int
handleTrianglePacket = CC.map packetToCount =$= CC.sum

packetToCount :: Maybe Packet -> Int
packetToCount Nothing               = 0
packetToCount (Just (Packet a b c)) = sum validTris
  where
    validTris = map (validateTriangle . T.unwords) . transpose $
                  [T.words a, T.words b, T.words c]
