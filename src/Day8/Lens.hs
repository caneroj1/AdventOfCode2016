module Day8.Lens where

import Control.Lens
import Data.List
import Data.Vector (Vector, (//))
import qualified Data.Vector as V hiding (Vector, (//))
import Day8.Types

trans :: Iso' (Vector (Vector a)) (Vector (Vector a))
trans = iso vl vl
  where lv = V.fromList . map V.fromList
        vl = lv . transpose . V.toList . V.map V.toList

matrix :: Iso' Matrix (Vector (Vector Pixel))
matrix = iso fromMatrix M
