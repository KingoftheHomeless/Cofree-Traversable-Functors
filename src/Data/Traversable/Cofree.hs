{-# LANGUAGE DataKinds, DeriveTraversable, GADTs #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, StandaloneDeriving #-}
module Data.Traversable.Cofree (
  module Data.Nat,
  module Data.Fin,
  module Data.Vec,
  module Data.Traversable,
  Cotra(..),
  unit,
  counit,
  hoist,
  fromTraversal
  ) where

import Data.Batch
import Data.Nat
import Data.Fin
import Data.Vec
import Data.Traversable

-- | The representational encoding of Cofree Traversable Functors.
-- Members of this type are representational triples.
data Cotra f a where
  Cotra :: SNat n -> f (Fin n) -> Vec n a -> Cotra f a

deriving instance Functor (Cotra f)
deriving instance Foldable (Cotra f)
deriving instance Traversable (Cotra f)

-- | Calculats the characterization of any traversable container,
-- effectively decoupling the elements of the container from
-- its shape.
unit :: Traversable t => t a -> Cotra t a
unit = fromTraversal traverse

-- | Integrates the elements into the shape, creating a container of the base functor.
counit :: Functor f => Cotra f a -> f a
counit (Cotra _ s l) = fmap (index l) s

-- | Applies the natural transformation to the shape of a triple.
hoist :: (forall x. f x -> g x) -> Cotra f a -> Cotra g a
hoist nat (Cotra n s l) = Cotra n (nat s) l

-- | Like 'unit', but the traversal under which the characterization
-- is calculated is provided explicitly.
fromTraversal :: forall s f a.
                 (    forall x.
                      (a -> Batch a x x)
                   -> s
                   -> Batch a x (f x)
                 )
              -> s
              -> Cotra f a
fromTraversal tr s =
  let
    g :: forall x. Batch a x (f x)
    g = tr batch s
  in
    case g of
      P h -> Cotra SZ h End
      r :*: a -> case makeElems r (SomeNEVec (a :- End)) of
        SomeNEVec (elems :: Vec ('S n) a) ->
          let
            sz :: SNat n
            SS sz = vecToSNat elems

            shape :: f (Fin ('S n))
            shape = makeShape (toFin sz) g
          in
            Cotra (SS sz) shape elems
  where
    makeElems :: Batch a b c -> SomeNEVec a -> SomeNEVec a
    makeElems (r :*: a) (SomeNEVec v) =
      makeElems r (SomeNEVec (a :- v))
    makeElems _         v             =
      v

    makeShape :: Fin ('S n) -> Batch a (Fin ('S n)) c -> c
    makeShape FZ (P l :*: _)   =
      l FZ
    makeShape (FS n) (r :*: _) =
      makeShape (raise n) r (FS n)
    makeShape _      _         =
      error "Impossible: elements of g dependent on type"
