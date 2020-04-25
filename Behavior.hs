module Behavior(Behavior', fromBehavior, toBehavior) where

import Reactive.Banana

data Behavior' a = Pure !a | Impure !(Behavior a) deriving Functor

instance Applicative Behavior' where
  pure = Pure
  Pure f <*> Pure a = Pure (f a)
  Pure f <*> Impure a = Impure (f <$> a)
  Impure f <*> Pure a = Impure (($ a) <$> f)
  Impure f <*> Impure a = Impure (f <*> a)

fromBehavior :: Behavior a -> Behavior' a
fromBehavior = Impure

toBehavior :: Behavior' a -> Behavior a
toBehavior (Pure a) = pure a
toBehavior (Impure a) = a
