{-# LANGUAGE LambdaCase   #-}

module Loop (
  loop
) where

import "GLFW-b" Graphics.UI.GLFW
import Control.Category
import Data.Label as L hiding (modify)
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
import Bindings

loop :: Network -> Game ()
loop network@(smp, snk)
  = unlessM (isStopped)
    <| do
      io <| threadDelay 10000

      conf <- gets wconf

      -- process input
      io <| readInput network conf

      -- execute actions
      io smp >>= sequence

      -- pause if necessary
      whenM (isStatus Paused) reLoop

      -- debug
      io <| putStrLn "Squares alives"
      gets squares >>= io <. print <. alives

      -- render Frame
      renderFrame

      -- loop again
      reLoop

      where
        reLoop = loop network

-- Input computing

isInputOn :: Window -> InputSource -> IO Bool
isInputOn window (KeyboardS key) =
  getKey window key
    $>> \case
      KeyState'Pressed    -> True
      KeyState'Repeating -> True
      _                          -> False
isInputOn window (MouseS button) =
  getMouseButton window button
    $>> \case
      MouseButtonState'Pressed -> True
      _                                    -> False


readInput :: Network -> WConf  -> IO ()
readInput network@(smp, snk) conf
  = do
    let win = get window conf
    let status = get runStatus conf
    let b = get bindings conf

    pollEvents

    inputs <- filterM (
      \(i, s) ->
        isInputOn win i
        $>> (\b -> b && case s of
          (Just s)  -> (s == status)
          Nothing -> True)
      ) <| M.keys b

    forM_ inputs
      <| \k -> snk <. fromJust
      <| M.lookup k b
    --
    -- -- mouse
    -- buttonsPressed <- filterM (isPressed win . MouseS)
    --   <| [MouseButton'1]
    --
    -- curPos <- getCursorPos win
    --
    -- forM_ buttonsPressed
    --   <| \b -> snk <. fromJust
    --   <| M.lookup b (mouseBindings conf curPos)
