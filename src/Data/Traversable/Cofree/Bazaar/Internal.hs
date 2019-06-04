{-# LANGUAGE DeriveFunctor, RankNTypes #-}
module Data.Traversable.Cofree.Bazaar.Internal where

import Control.Applicative (liftA2)

newtype Bazaar a s t =
  Bazaar { runBazaar :: forall f. Applicative f => (a -> f s) -> f t }
  deriving (Functor)

instance Applicative (Bazaar a s) where
  pure a = Bazaar $ \_ -> pure a
  ff <*> fa = Bazaar $ \c -> runBazaar ff c <*> runBazaar fa c
  liftA2 f fa fb = Bazaar $ \c -> liftA2 f (runBazaar fa c) (runBazaar fb c)

sell :: a -> Bazaar a s s
sell a = Bazaar (\c -> c a)
