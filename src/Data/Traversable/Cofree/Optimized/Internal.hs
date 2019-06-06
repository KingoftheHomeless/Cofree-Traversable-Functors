{-# LANGUAGE DeriveTraversable, GADTs, RankNTypes, StandaloneDeriving #-}
module Data.Traversable.Cofree.Optimized.Internal where

import Control.Applicative (liftA2)

data FunList a b t where
  Done :: t -> FunList a b t
  More :: (x -> b -> t) -> a -> FunList a b x -> FunList a b t

deriving instance Functor (FunList a b)

instance Applicative (FunList a b) where
  pure = Done
  Done f <*> fa = fmap f fa
  More ex a r <*> fa = More id a (liftA2 (flip . ex) r fa)

  liftA2 f (Done a) fb = fmap (f a) fb
  liftA2 f (More ex s r) fb = More id s (liftA2 (\x b c -> f (ex x c) b) r fb)

flpure :: a -> FunList a b b
flpure a = More (\_ -> id) a (Done ())

{-|
  Coalesce is a strange applicative transformer that effectively
  "right-associates" uses of '<*>', and may therefore be used to
  eliminate the poor behaviour of 'FunList' when used together
  with traversals that have "left-associative" uses of '<*>'.

  It also merges uses of multiple uses of 'fmap' into a single one.
-}
newtype Coalesce f a =
  Coalesce {
    runCoalesce :: forall x y.
                   (a -> x -> y)
                -> (forall z. (x -> z) -> f z)
                -> f y
    }

instance Functor (Coalesce f) where
  fmap f coal =
    Coalesce $ \bxy -> runCoalesce coal (bxy . f)

instance Applicative (Coalesce f) where
  pure a = Coalesce $ \axy c -> c (axy a)
  ff <*> fa = Coalesce $ \bxy c ->
    runCoalesce ff (\f ~(a, x) -> bxy (f a) x) $ \tz ->
      runCoalesce fa (\a x -> tz (a, x)) c

  liftA2 f fa fb = Coalesce $ \cxy c ->
    runCoalesce fa (\a ~(b, x) -> cxy (f a b) x) $ \tz ->
      runCoalesce fb (\b x -> tz (b, x)) c

liftCoalesce :: Applicative f => f a -> Coalesce f a
liftCoalesce fa = Coalesce $ \f c -> liftA2 f fa (c id)

lowerCoalesce :: Applicative f => Coalesce f a -> f a
lowerCoalesce coal = runCoalesce coal const (\c -> pure (c ()))
