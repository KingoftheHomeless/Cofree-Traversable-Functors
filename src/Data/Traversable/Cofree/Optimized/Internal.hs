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
  Coalesce { runCoalesce :: forall x. (forall y. ((a -> x) -> y) -> f y) -> f x}

instance Functor (Coalesce f) where
  fmap f coal =
    Coalesce $ \c -> runCoalesce coal $ \axy -> c $ \bx -> axy (bx . f)

instance Applicative (Coalesce f) where
  pure a = Coalesce $ \c -> c (\ax -> ax a)
  ff <*> fa = Coalesce $ \c ->
    runCoalesce ff $ \fxy ->
      runCoalesce fa $ \ayz ->
        c $ \bx ->
          ayz $ \a ->
           fxy $ \f ->
             bx (f a)

  liftA2 f fa fb = Coalesce $ \cn ->
    runCoalesce fa $ \axy ->
      runCoalesce fb $ \byz ->
        cn $ \cx ->
         byz $ \b ->
           axy $ \a ->
             cx (f a b)

liftCoalesce :: Applicative f => f a -> Coalesce f a
liftCoalesce fa = Coalesce $ \c -> liftA2 (\a f -> f a) fa (c id)

lowerCoalesce :: Applicative f => Coalesce f a -> f a
lowerCoalesce coal = runCoalesce coal (\c -> pure (c id))
