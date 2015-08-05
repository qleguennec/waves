module Bindings (
  InputSource(..)
  , defaultBindings
  , Bindings
) where

import "GLFW-b" Graphics.UI.GLFW
import Data.Label as L hiding (modify)
import Data.Label.Monadic as St
import qualified Data.Map as M
import Control.Category
import Prelude hiding ((.), id)
import Flow

import World
import Util

defaultBindings :: Bindings
defaultBindings = M.fromList
  <| merge
  ([
    (KeyboardS Key'Escape,  [
      (Nothing, stop)
    ])

    , (KeyboardS Key'Space, [
      (Just Running, pause)
      , (Just Paused, run)
    ])
  ]
  ++
  [
    (MouseS MouseButton'1, [
      (Just Running, squareClick)
    ])
  ])
  where
    squareClick :: Game ()
    squareClick = do
      curPos <- gets (window . wconf) >>= io <. getCursorPos
      squaresize <- gets (squareSize . wconf)
      let c = getSquareCoord curPos squaresize
      io <| print c
      retrieveSquare c >>= updateSquare



merge :: [(a, [(b, c)])] -> [((a, b), c)]
merge [] = []
merge ((a, i):r) = (map (\(b, c) -> ((a, b), c)) i) ++ merge r
