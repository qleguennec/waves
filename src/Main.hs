module Main where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Monad.State hiding (get, put, modify)
import Graphics.Gloss.Rendering
import Control.Category
import Data.Label
import Control.Monad (when, void)
import Prelude hiding ((.), id)
import Flow
import FRP.Elerea.Simple
import qualified Data.Map as M


import World
import Render
import Loop

defWConf :: WConf
defWConf = WConf {
  _width = 640
  , _height = 480
  , _title = "test"
  , _squareSize = 20
}

withWindow :: WConf -> (GLFW.Window -> IO ()) -> IO ()
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
        act win
        GLFW.destroyWindow win
  GLFW.terminate
  where
    simpleErrorCallback e s =
      putStrLn <| unwords [show e, show s]

main :: IO ()
main = withWindow defWConf
    <| \window -> do
      -- gloss initialization
      !glossState <- initState

      -- network initialization
      !network <- initNetwork

      -- start the loop
      void <| flip execStateT
        (initialWorld glossState (get squareSize defWConf))
        <| do
          renderFrame defWConf window
          loop network defWConf window

  where
    (w, h) = get wCoord <| relative BottomLeft defWConf
    (w', h') = get wCoord <| relative TopRight defWConf

    initialWorld glossState squareSize = World {
      _stopped = False
      , _state = glossState
      , _squares = squareList squareSize
    }

    squareList :: Int -> [Square]
    squareList size = [
      Square x y False
      | x <- [w, size+w..w']
      , y <- [h, size+h..h']]

    initNetwork :: IO Network
    initNetwork = do
       (smp, snk) <- externalMulti
       netw <- start smp
       return (netw, snk)
