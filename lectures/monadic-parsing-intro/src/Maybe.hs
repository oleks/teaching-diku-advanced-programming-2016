{-# LANGUAGE NoImplicitPrelude #-}

--import Control.Applicative
--import Control.Monad

import Data.String

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  fail :: String -> m a
  fail = error

data Maybe a = Just a | Nothing

instance Monad Maybe where
  return a = Just a
  Just a >>= f = f a
  Nothing >>= _ = Nothing
  fail _ = Nothing

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Applicative Maybe where
  pure = Just
  Nothing  <*> _ = Nothing
  (Just f) <*> something = fmap f something
