{-# LANGUAGE DeriveTraversable, RankNTypes, ScopedTypeVariables #-}
module Data.Traversable.Cofree.Bazaar where

import Data.Coerce
import Data.Functor.Const
import Data.Functor.Compose
import Data.Functor.Identity
import Unsafe.Coerce

import Data.Traversable.Cofree.Bazaar.Internal

newtype CotraBazaar f a =
  Baz { runBaz :: forall g x. Applicative g => (a -> g x) -> g (f x) }
  deriving (Functor)

instance Foldable (CotraBazaar f) where
  -- Small optimization compared to foldMapDefault
  foldMap (f :: a -> m) bz = getConst (runBaz bz (coerce f :: a -> Const m ()))

instance Traversable (CotraBazaar f) where
  traverse f baz =
    fmap unsafeCoerce . getCompose
      $ runBaz baz (\a -> Compose (sell <$> f a))

unitBazaar :: Traversable t => t a -> CotraBazaar t a
unitBazaar = fromTraversalBazaar traverse

counitBazaar :: CotraBazaar t a -> t a
counitBazaar baz = runIdentity (runBaz baz Identity)

hoistBazaar :: (forall x. f x -> g x)
            -> CotraBazaar f a -> CotraBazaar g a
hoistBazaar nt baz = Baz $ \c -> nt <$> runBaz baz c

fromTraversalBazaar :: forall s f a.
                 (    forall g x.
                      Applicative g
                   => (a -> g x)
                   -> s
                   -> g (f x)
                 )
              -> s
              -> CotraBazaar f a
fromTraversalBazaar tr s = Baz (`tr` s)

