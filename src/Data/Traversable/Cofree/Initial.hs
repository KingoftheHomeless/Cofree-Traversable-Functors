{-# LANGUAGE DeriveTraversable, GADTs, RankNTypes, StandaloneDeriving #-}
module Data.Traversable.Cofree.Initial where

import Data.Traversable.Cofree.Bazaar

-- The Initial encoding
data CotraInitial f a where
  Initial :: Traversable t
          => (forall x. t x -> f x)
          -> t a -> CotraInitial f a

deriving instance Functor (CotraInitial f)
deriving instance Foldable (CotraInitial f)
deriving instance Traversable (CotraInitial f)

unitInitial :: Traversable t => t a -> CotraInitial t a
unitInitial = Initial id

counitInitial :: CotraInitial f a -> f a
counitInitial (Initial ex ta) = ex ta

hoistInitial :: (forall x. f x -> g x)
             -> CotraInitial f a -> CotraInitial g a
hoistInitial nt (Initial ex ta) = Initial (nt . ex) ta

-- This encoding may only implement "fromTraveral" by working through another encoding.
fromTraversalInitial :: forall s f a.
                 (    forall g x.
                      Applicative g
                   => (a -> g x)
                   -> s
                   -> g (f x)
                 )
              -> s
              -> CotraInitial f a
fromTraversalInitial tr s = Initial counitBazaar (fromTraversalBazaar tr s)
