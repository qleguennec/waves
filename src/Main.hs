module Main where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Monad.State hiding (get, put, modify, state)
import Graphics.Gloss.Rendering
import Graphics.Gloss.Rendering as Gloss (State)
import Control.Category
import Data.Label
import Data.Label.Monadic
import Control.Monad (when, void)
import Prelude hiding ((.), id)
import Flow
import FRP.Elerea.Simple
import qualified Data.Map as M


import World
import Render
import Loop
import Bindings

main :: IO ()
main = do
  -- pre-window initializations
  glossState <- initState
  let defWConf' = set state glossState defWConf

  -- window creation
  withWindow defWConf'
    <| \conf -> do
      -- retrieve window
      let win = get window conf

      -- network initialization
      !network <- initNetwork

      -- start the loop
      void <| flip execStateT
        (initialWorld conf)
        <| do
          renderFrame
          loop network

  where
    (w, h) = get wCoord <| relative BottomLeft defWConf
    (w', h') = get wCoord <| relative TopRight defWConf

    -- Window configuration
    defWConf :: WConf
    defWConf = WConf {
      _window = undefined
      , _state = undefined
      ,  _bindings = defaultBindings
      , _runStatus = Paused
      , _width = 640
      , _height = 480
      , _title = "test"
      , _squareSize = 20
    }

    -- initial world configuration
    initialWorld conf =
      let s = get squareSize conf
      in World {
        _wconf = conf
        , _squares = squareList s
      }

    squareList :: Int -> [Square]
    squareList size = [
        Square x y False
        | x <- [0.. w `div` size]
        , y <- [0.. h `div` size]
      ]

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
