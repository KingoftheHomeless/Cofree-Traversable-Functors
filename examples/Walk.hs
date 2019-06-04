{-# LANGUAGE DataKinds, DeriveFunctor, GADTs, KindSignatures #-}
{-# LANGUAGE StandaloneDeriving, TypeFamilies, UndecidableInstances #-}
module Walk where

import Control.Applicative.Backwards
import Data.Foldable (foldl')
import Data.Traversable.Cofree

type family Add (n :: Nat) (m :: Nat) where
  Add 'Z m = m
  Add ('S n) m = Add n ('S m)

-- | Appends the elements of the left vector to the right in reverse order.
reverseApp :: Vec n a -> Vec m a -> Vec (Add n m) a
reverseApp End      r = r
reverseApp (a :- l) r = reverseApp l (a :- r)


{- |
A GADT that may be used together with 'Cotra' in order
to allow an additional number of "holes" in the predefined
context of the parametrized functor.
-}
data Partial (n :: Nat) f a where
  Partial :: f (Fin (Add n m)) -> Partial n f (Fin m)

-- | A bidirectional zipper for any arbitrary traversable, which is itself traversable.
data Walk t a where
  Walk :: SNat n -> Vec n a -> Cotra (Partial n t) a -> Walk t a

deriving instance Functor (Walk t)

instance Foldable (Walk t) where
  foldr c b (Walk _ l r) = foldl' (flip c) (foldr c b r) l

instance Traversable (Walk t) where
  traverse f (Walk n l t) =
        Walk n
    <$> forwards (traverse (Backwards . f) l)
    <*> traverse f t

unzip :: Traversable t => t a -> Walk t a
unzip t = case unit t of
  Cotra n s l -> Walk SZ End (Cotra n (Partial s) l)

rezip :: Functor t => Walk t a -> t a
rezip (Walk _ ll (Cotra _ (Partial s) rl)) =
  index (reverseApp ll rl) <$> s

right :: Walk t a -> Maybe (Walk t a)
right (Walk ln ll (Cotra (SS rn) (Partial s) (a :- rl))) =
  Just $
    Walk
      (SS ln)
      (a :- ll)
      (Cotra rn (Partial s) rl)
right _ = Nothing

left :: Walk t a -> Maybe (Walk t a)
left (Walk (SS ln) (a :- ll) (Cotra rn (Partial s) rl)) =
  Just $
    Walk
      ln
      ll
      (Cotra (SS rn) (Partial s) (a :- rl))
left _ = Nothing

focus :: Walk t a -> Maybe a
focus (Walk _ _ (Cotra _ _ (a :- _))) =
  Just a
focus _ =
  Nothing

write :: a -> Walk t a -> Maybe (Walk t a)
write a (Walk ln ll (Cotra rn s (_ :- rl))) =
  Just $
    Walk
      ln
      ll
      (Cotra rn s (a :- rl))
write _ _ =
  Nothing
