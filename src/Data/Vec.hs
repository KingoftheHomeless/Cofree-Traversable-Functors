{-# LANGUAGE DataKinds, DeriveTraversable, GADTs, StandaloneDeriving #-}
module Data.Vec where

import Data.Foldable (toList)
import Data.Nat
import Data.Fin

data Vec n a where
  End  :: Vec 'Z a
  (:-) :: a -> Vec n a -> Vec ('S n) a

infixr 5 :-

deriving instance Functor (Vec n)
deriving instance Foldable (Vec n)
deriving instance Traversable (Vec n)

instance Show a => Show (Vec n a) where
  showsPrec i = showsPrec i . toList

index :: Vec n a -> Fin n -> a
index (a :- _) FZ     = a
index (_ :- r) (FS i) = index r i
index End      g      = absurdFin g

vecToSNat :: Vec n a -> SNat n
vecToSNat End      = SZ
vecToSNat (_ :- r) = SS (vecToSNat r)

data SomeNEVec a where
  SomeNEVec :: Vec ('S n) a -> SomeNEVec a
