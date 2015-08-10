module Play.Conway (
  conway
) where

import Control.Category
import Data.Label.Monadic as St
import Data.Label as L
import Data.Maybe (catMaybes)
import qualified Data.Foldable as F
import qualified Data.Set as S
import Flow

import World
import Util

conway :: Game ()
conway = transformS <|
  \s -> (S.size <$> neighboursAlive s) >>= \case
      3 -> life s
      2 -> return ()
      _ -> death s

neighbours :: Square -> Game Squares
neighbours s = S.fromList <$> catMaybes <$> mapM retrieveSquare
  [
    (x+1, y+1)
    , (x+1, y)
    , (x+1, y-1)
    , (x, y+1)
    , (x, y-1)
    , (x-1, y+1)
    , (x-1, y)
    , (x-1, y-1)
  ]
  where
    (x, y) = get coord s

neighboursAlive :: Square -> Game Squares
neighboursAlive s = alives <$> neighbours s
