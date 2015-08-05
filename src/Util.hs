module Util where

import           Control.Applicative
import           Control.Arrow       ((***))
import           Control.Monad       (join)
import Control.Monad.State.Strict (MonadIO, liftIO)
import Control.Concurrent (threadDelay)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM a f g = do
  cond <- a
  if cond then f else g

whenM :: Monad m => m Bool -> m () -> m ()
whenM a f = ifM a f (return ())

notM :: Monad m => m Bool -> m Bool
notM = (=<<) (return . not)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM = whenM . notM

io :: MonadIO m => IO a -> m a
io = liftIO

($>>) :: Functor f => f a -> (a -> b) -> f b
($>>) = flip (<$>)

toFloat :: Int -> Float
toFloat = fromIntegral

toInt :: Float -> Int
toInt = floor

both :: (a -> b) -> (a, a) -> (b, b)
both = join (***)

untilIO :: Int -> IO Bool -> IO a -> IO a
untilIO d p k = ifM p k (threadDelay d >> untilIO d p k)

equalsM ::(Eq a, Monad m) => m a -> a -> m Bool
equalsM k a = (== a) <$> k
