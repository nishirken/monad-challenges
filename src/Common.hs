module Common where

class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: m a
