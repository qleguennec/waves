module Main where

import Criterion.Main
import Criterion.Main.Options
import Criterion.Types
import System.Random
import qualified Data.Label.Monadic as St
import Control.Monad.State.Strict
import Prelude hiding ((.), id)
import Control.Category

import World
import Util
import Boot

instance (Random x, Random y) => Random (x, y) where
  random = undefined
  randomR ((x1, y1), (x2, y2)) gen1 =
    let (x, gen2) = randomR (x1, x2) gen1
        (y, gen3) = randomR (y1, y2) gen2
    in ((x, y), gen3)

-- careful with that headache eugene
main :: IO ()
main = defaultMainWith
  defaultConfig {
    verbosity = Verbose
  }

  [
    bench "Squares updates" $ whnfIO $ execGame
      $ do
        gen <- io getStdGen
        (w, h) <- St.gets (wCoord . wconf)
          $>> both (\a -> fromIntegral a :: Double)

        forM_ (take 1000 $ randomRs ((0, w), (0, h)) gen)
          $ \c -> do
            size <- St.gets (squareSize . wconf)
            (w, h) <- St.gets (wCoord . wconf)
            retrieveSquare (getSquareCoord c (w, h) size)
            >>= updateSquare
  ]
