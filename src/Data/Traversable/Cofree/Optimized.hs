{-# LANGUAGE DeriveTraversable, GADTs, RankNTypes, StandaloneDeriving #-}
module Data.Traversable.Cofree.Optimized where

import Unsafe.Coerce

import Data.Traversable.Cofree.Optimized.Internal

data CotraOpt f a where
   Base :: (forall x. f x) -> CotraOpt f a
   Layer :: (forall x. g x -> x -> f x)
         -> a
         -> CotraOpt g a
         -> CotraOpt f a

deriving instance Functor (CotraOpt f)
deriving instance Foldable (CotraOpt f)
deriving instance Traversable (CotraOpt f)

unitOpt :: Traversable t => t a -> CotraOpt t a
unitOpt = fromTraversal traverse

-- | A variant of 'unitOpt' that uses 'fromTraversal'' internally.
unitOpt' :: Traversable t => t a -> CotraOpt t a
unitOpt' = fromTraversal' traverse

counitOpt :: CotraOpt f a -> f a
counitOpt (Base t) = t
counitOpt (Layer ex a r) = ex (counitOpt r) a

hoistOpt :: (forall x. f x -> g x)
          -> CotraOpt f a
          -> CotraOpt g a
hoistOpt n (Base t) = Base (n t)
hoistOpt n (Layer ex a r) = Layer (\x h -> n (ex x h)) a r

fromTraversal :: forall s f a.
                 (    forall x.
                      (a -> FunList a x x)
                   -> s
                   -> FunList a x (f x)
                 )
              -> s
              -> CotraOpt f a
fromTraversal tr t = go (tr flpure t)
  where
    go :: (forall x. FunList a x (u x)) -> CotraOpt u a
    go (Done t') = Base (unsafeCoerce t')
    go (More ex a r) = Layer (unsafeCoerce ex) a (go (unsafeCoerce r))

-- | A variant of fromTraversal that takes care to right-associate uses of '<*>'
-- in the traversal, thereby ensuring 'FunList' performs optimally,
-- at the cost of a small overhead per use of '<*>' within the traversal.
-- I believe this is guaranteed to have the same time complexity as the traversal itself.
fromTraversal' :: forall s f a.
                 (    forall x.
                      (a -> Coalesce (FunList a x) x)
                   -> s
                   -> Coalesce (FunList a x) (f x)
                 )
              -> s
              -> CotraOpt f a
fromTraversal' tr = fromTraversal (\f -> lowerCoalesce . tr (liftCoalesce . f))
