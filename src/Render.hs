module Render (
  renderFrame
) where

import Prelude hiding ((.), id)
import Control.Category
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss.Rendering
import Data.Label
import Data.Label.Monadic
import Flow

import World
import Util

renderFrame :: WConf -> GLFW.Window -> Game ()
renderFrame conf window = do
  glossState <- gets state
  picture <- Pictures
    <$> gets squares
    $>> map (\square -> let c = get coordFloat square
      in Pictures
      [
        fill square <| Polygon (path c s)
        , border square <| Line (path c s)
      ])
  io <! displayPicture (640, 480) background glossState 1.0 picture
  io <! swapBuffers window
  where
    s = get (float . squareSize) conf
    path (a, b) s = [(a, b), (a+s, b), (a+s, b-s),  (a, b-s)]

background :: Color
background = makeColorI 0 0 0 0

fill :: Square -> Picture -> Picture
fill square = Color <|
  if (get alive square)
  then full
  else empty
  where
    full = makeColorI 255 255 255 255
    empty = makeColorI 0 0 0 0

border :: Square -> Picture -> Picture
border square = Color <|
  if (get alive square)
  then full
  else empty
  where
    full = makeColorI 255 255 255 255
    empty = makeColorI 255 255 255 255
