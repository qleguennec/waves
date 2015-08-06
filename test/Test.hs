module Main where

import Boot
import Util
import World hiding (run)

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Monadic
import System.Random
import Control.Monad.State.Strict hiding (get, put, modify, state)
import Data.Label.Monadic as St
import Control.Category
import Prelude hiding ((.), id)

main :: IO ()
main = quickCheck prop_square

-- here's some really dirty code, Eugene.
prop_square = monadicIO $ do
  (a, b) <- run $ (,)
    <$> (generate $ choose (10 :: Int, 10000 :: Int))
    <*> ( generate $ choose (10 :: Int, 10000 :: Int))
  (x, y) <- run $ both (\a -> fromIntegral a :: Double)
    <$> ((,)
    <$> (generate $ choose (0, a))
    <*> (generate $ choose (0, b)))

  b <- run (flip evalStateT (initialWorld
    $ defWConf {
      _width = a
      , _height = b
    })
   $ do
    square <- (St.gets (squareSize . wconf)
      $>> getSquareCoord (x, y) (a, b))
      >>= retrieveSquare
    case square of
      Nothing -> return False
      (Just s) -> do
        let life = isAlive s
        updateSquare (Just s)
        square' <- (St.gets (squareSize . wconf)
          $>> getSquareCoord (x, y) (a, b))
          >>= retrieveSquare
        case square' of
          Nothing -> return False
          (Just s') -> do
            let life' = isAlive s'
            return $ life /= life')
  assert b
