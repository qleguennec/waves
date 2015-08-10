module World where

import "GLFW-b" Graphics.UI.GLFW as GLFW (Window)
import Prelude hiding ((.), id)
import Control.Monad.State.Strict hiding (get, modify, gets)
import Control.Category
import Control.Applicative
import Graphics.Gloss.Rendering as Gloss (State)
import Data.List (find, delete, insert)
import Data.Label as L
import qualified Data.Foldable as F
import qualified Data.Set as S
import Data.Label.Monadic as St
import qualified Data.Map.Strict as M
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Concurrent.STM
import System.Random
import Flow

import Util

-- custom types
type Game a = StateT World IO a
type Network = (IO [Game ()], Game () -> IO ())
data InputSource = KeyboardS Key | MouseS MouseButton
  deriving (Ord, Eq)
type Bindings = M.Map (InputSource, Maybe (RunStatus)) (Game ())

-- data
data Square = Square {
  _x :: Int
  , _y :: Int
  , _alive :: Bool
} deriving (Ord, Eq, Show)

data Player = Player {
  _pos :: Int
} deriving Eq

data RunStatus = Running | Paused | Stopped | Debug
  deriving (Eq, Show, Ord)

type Squares = S.Set Square

data World = World {
  _wconf :: WConf
  , _squares :: TVar Squares
}

data WConf = WConf {
  _window :: Window
  , _state     :: Gloss.State
  , _bindings :: Bindings
  , _runStatus :: RunStatus
  , _width :: Int
  , _height :: Int
  , _title :: String
  , _squareSize :: Int
}

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

-- actions on squares
readS :: Game Squares
readS = St.gets squares
  >>= (perform <. readTVar)

writeS :: Squares -> Game ()
writeS s = St.gets squares
  >>= (perform <. flip writeTVar s)

alterS :: (Squares -> Squares) -> Game ()
alterS f = St.gets squares
  >>= (perform <. flip modifyTVar f)

transformS :: (Square -> Game ()) -> Game ()
transformS = (readS >>=) <. F.mapM_

mapS :: (Square -> a) -> Game [a]
mapS f = (map f <. F.toList) <$> readS

getSquareCoord :: (Double, Double) -> (Int, Int) -> Int -> (Int, Int)
getSquareCoord (a, b) (w, h) squaresize =
  (   (floor a - w `div` 2) `div` squaresize
  , - (floor b - h `div` 2) `div` squaresize
  )

retrieveSquare :: (Int, Int) -> Game (Maybe Square)
retrieveSquare (a, b) = readS
  $>> find (\s -> (a == get x s) && (b == get y s))

setAlive :: Bool -> Square -> Game ()
setAlive l s = do
  alterS <| S.delete s
  alterS <| (S.insert <| L.set alive l s)

life, death :: Square -> Game ()
life = setAlive True
death = setAlive False

updateSquare :: Square -> Game ()
updateSquare s  = do
  alterS <| S.delete s
  alterS <| (S.insert <| L.modify alive not s)

randomize :: Game ()
randomize = do
  gen <- io newStdGen
  transformS <| setAlive (fst <| random gen)

isAlive :: Square -> Bool
isAlive = get alive

alives :: Squares -> Squares
alives = S.filter isAlive

-- Run status control
isStatus :: RunStatus -> Game Bool
isStatus s = gets (runStatus . wconf) $>> (== s)

isPaused, isRunning, isStopped :: Game Bool
isPaused  = isStatus Paused
isRunning = isStatus Running
isStopped = isStatus Stopped
isDebug = isStatus Debug

updateStatus :: RunStatus -> Game ()
updateStatus s = puts (runStatus . wconf) s

pause, run, stop :: Game ()
pause = updateStatus Paused
run = updateStatus Running
stop = updateStatus Stopped
debug = updateStatus Debug
