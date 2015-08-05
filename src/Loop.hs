{-# LANGUAGE LambdaCase   #-}

module Loop (
  loop
) where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Category
import Data.Label as L
import Data.Label.Monadic as St
import Data.Maybe (fromJust)
import Control.Monad (when, void, filterM, forM_)
import Control.Concurrent (threadDelay)
import Prelude hiding ((.), id)
import Flow
import qualified Data.Map as M
import FRP.Elerea.Simple

import World
import Render
import Util

loop :: Network -> WConf -> GLFW.Window -> Game ()
loop network@(smp, snk) conf window
  = unlessM (gets stopped)
    <| do

      -- process input
      io <| readInput network conf window

      -- execute actions
      actions <- io smp >>= sequence

      -- debug
      io <| putStrLn "Squares alives"
      gets squares >>= io <. print <. alives

      -- render Frame
      renderFrame conf window

      -- loop again
      io <| threadDelay 10000
      loop network conf window

-- Input computing

type KeyBindings = M.Map Key (Game ())
keyBindings :: KeyBindings
keyBindings = M.fromList
  [
    (Key'Escape,  puts stopped True)
  ]

type MouseBindings = M.Map MouseButton (Game ())
mouseBindings :: WConf -> (Double, Double) -> MouseBindings
mouseBindings conf click = M.fromList
  [
    (MouseButton'1,  squareClick click)
  ]
  where
    size = get squareSize conf

    squareClick :: (Double, Double) -> Game ()
    squareClick input =
      -- (retrieveSquare <|
      --   getSquareCoord input size
      -- ) >>= updateSquare

      io <| print <| getSquareCoord input size

data InputSource = MouseS MouseButton | KeyboardS Key

isPressed :: GLFW.Window -> InputSource -> IO Bool
isPressed window (KeyboardS key) =
  getKey window key
    $>> \case
      KeyState'Pressed    -> True
      KeyState'Repeating -> True
      _                          -> False
isPressed window (MouseS button) =
  getMouseButton window button
    $>> \case
      MouseButtonState'Pressed -> True
      _                                    -> False


readInput :: Network -> WConf -> GLFW.Window -> IO ()
readInput network@(smp, snk) conf window
  = do
    pollEvents

    -- keyboard
    keysPressed <- filterM (isPressed window . KeyboardS)
      <| M.keys keyBindings

    forM_ keysPressed
      <| \k -> snk <. fromJust
      <| M.lookup k keyBindings

    -- mouse
    buttonsPressed <- filterM (isPressed window . MouseS)
      <| [MouseButton'1]

    curPos <- getCursorPos window

    forM_ buttonsPressed
      <| \b -> snk <. fromJust
      <| M.lookup b (mouseBindings conf curPos)
