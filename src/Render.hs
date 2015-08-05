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

renderFrame :: Game ()
renderFrame = do
  glossState <- gets (state . wconf)
  size <- gets (float . squareSize . wconf)
  win <- gets (window . wconf)
  picture <- Pictures
    <$> gets squares
    $>> map (\square ->
      let c = get coordFloat square |> both (*size)
      in Pictures
      [
        fill square <| Polygon (path c size)
        , border square <| Line (path c size)
      ])
  io <! displayPicture (640, 480) background glossState 1.0 picture
  io <! swapBuffers win
  where
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
