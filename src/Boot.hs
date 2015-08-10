module Boot (
  startGame
  , execGame
  , initialWorld
  , defWConf
) where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Monad.State.Strict hiding (get, put, modify, state)
import Graphics.Gloss.Rendering as Gloss (initState)
import Data.Label
import qualified Data.Set as S
import Prelude hiding ((.), id)
import Flow
import FRP.Elerea.Simple
import Control.Concurrent.STM
import System.IO.Unsafe

import World
import Render
import Loop
import Bindings

-- Window configuration
defWConf :: WConf
defWConf = WConf {
  _window = undefined
  , _state = undefined
  ,  _bindings = defaultBindings
  , _runStatus = Paused
  , _width = 640
  , _height = 480
  , _title = "Conway"
  , _squareSize = 20
}

-- initial world configuration
initialWorld :: WConf -> World
initialWorld conf =
  let s = get squareSize conf
  in World {
    _wconf = conf
    , _squares =
      unsafePerformIO <. newTVarIO <| squareList s
  }

squareList :: Int -> Squares
squareList size = S.fromList [
    Square x' y' False
    | x' <- [w `div` size .. w' `div` size]
    , y' <- [h `div` size .. h' `div` size]
  ]
  where
    (w, h) = get wCoord <| relative BottomLeft defWConf
    (w', h') = get wCoord <| relative TopRight defWConf

execGame :: World -> Game () -> IO ()
execGame w g = void <| execStateT g w

startGame :: IO ()
startGame = do
  -- pre-window initializations
  glossState <- Gloss.initState
  let defWConf' = set state glossState defWConf

  -- window creation
  withWindow defWConf'
    <| \conf -> do

      -- network initialization
      !network <- initNetwork

      -- start the loop
      execGame (initialWorld conf)
        <| do
          renderFrame
          loop network

  where
    initNetwork :: IO Network
    initNetwork = do
       (smp, snk) <- externalMulti
       netw <- start smp
       return (netw, snk)

withWindow :: WConf -> (WConf-> IO ()) -> IO ()
withWindow conf act = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  i <- GLFW.init
  when i <| do
    context <- GLFW.createWindow
      (get width conf)
      (get height conf)
      (get title conf)
      Nothing
      Nothing
    case context of
      Nothing   -> return ()
      (Just win) -> do
        GLFW.makeContextCurrent context
        act <| set window win conf
        GLFW.destroyWindow win
  GLFW.terminate
  where
    simpleErrorCallback e s =
      putStrLn <| unwords [show e, show s]
