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
import qualified Data.Map.Strict as M
import FRP.Elerea.Simple

import World
import Render
import Util
import Bindings

loop :: Network -> Game ()
loop network@(smp, snk)
  = unlessM isStopped
    <| do
      io <| threadDelay 100
      whenM isDebug debugMode
      conf <- gets wconf

      -- process input
      io <| readInput network conf

      -- execute actions
      io smp >>= sequence

      whenM isPaused reLoop

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
    >>= \case
      MouseButtonState'Pressed -> waitForRelease
      _                                    -> return False
  where
    waitForRelease = untilIO 2
      (pollEvents >>
        getMouseButton window button
        `equalsM` MouseButtonState'Released)
      <| return True

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

-- debugging utilities
debugMode :: Game ()
debugMode = do
  io <| print "DEBUG MODE - Enter expr"
  expr <- io <| getLine
  conf <- gets wconf
  case expr of
    "squares" -> gets squares >>= io <. print
    "wCoord" -> gets (wCoord . wconf) >>= io <. print
    "BL" -> io <. print <| (get wCoord <| relative BottomLeft conf)
    "TR" -> io <. print <| (get wCoord <| relative TopRight conf)
    't':s -> retrieveSquare (read s :: (Int, Int)) >>= updateSquare
    "exit" -> run
    "stop" -> stop
    _ -> return ()
