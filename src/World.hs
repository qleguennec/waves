module World where

import Prelude hiding ((.), id)
import Control.Monad.State hiding (get, modify)
import Control.Category
import Control.Applicative
import Graphics.Gloss.Rendering as Gloss (State)
import Data.List (find, delete, insert)
import Data.Label as L
import Data.Label.Monadic as St
import Flow

import Util

data Square = Square {
  _x :: Int
  , _y :: Int
  , _alive :: Bool
} deriving (Ord, Eq, Show)

data Player = Player {
  _pos :: Int
} deriving Eq

data World = World {
  _stopped :: Bool
  , _state     :: Gloss.State
  , _squares :: [Square]
}

data WConf = WConf {
  _width :: Int
  , _height :: Int
  , _title :: String
  , _squareSize :: Int
} deriving Eq

mkLabels [''Square, ''Player, ''World, ''WConf]
(fstL, sndL) = $(getLabel ''(,))

wCoord :: WConf :-> (Int, Int)
wCoord = point $
  (,) <$> fstL >- width
      <*> sndL >- height

data Corner = TopLeft | TopRight | BottomLeft | BottomRight

relative :: Corner -> WConf -> WConf
relative c = L.modify wCoord
  <| \(a, b) -> case c of
    TopLeft -> (-a, b)
    TopRight -> (a, b)
    BottomLeft -> (-a, -b)
    BottomRight -> (a, -b)

coord :: Square :-> (Int, Int)
coord = point $
  (,) <$> fstL >- x
      <*> sndL >- y

float = iso (Iso toFloat toInt)

coordFloat :: Square :-> (Float, Float)
coordFloat = point $
  (,) <$> fstL >- (float . x)
      <*> sndL >- (float . y)

-- custom types
type Game a = StateT World IO a
type Network = (IO [Game ()], Game () -> IO ())

-- actions
getSquareCoord :: (Double, Double) -> Int -> (Int, Int)
getSquareCoord (a, b) squaresize =
  ( (floor a `div` squaresize) * squaresize
  , (floor b `div` squaresize) * squaresize
  )

retrieveSquare :: (Int, Int) -> Game (Maybe Square)
retrieveSquare (a, b) = St.gets squares
  $>> find (\s -> (a == get x s) && (b == get y s))

updateSquare :: Maybe Square -> Game ()
updateSquare Nothing = return ()
updateSquare (Just s)  = do
  squares =. delete s
  squares =. (insert <| L.modify alive not s)

alives :: [Square] -> [Square]
alives = filter (get alive)
