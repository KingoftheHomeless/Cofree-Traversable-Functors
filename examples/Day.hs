{-# LANGUAGE GADTs, DeriveFunctor, StandaloneDeriving #-}
module Day where

import Data.Traversable.Cofree

data Day f g a where
  Day :: f x -> g y -> (x -> y -> a) -> Day f g a

deriving instance Functor (Day f g)

instance (Foldable f, Foldable g) =>
  Foldable (Day f g) where
  foldr c b (Day fx gy xya) =
    foldr (\x b' -> foldr (\y -> c (xya x y)) b' gy) b fx

instance (Traversable f, Traversable g)
      => Traversable (Day f g) where
  traverse f (Day fx gy xya) = case (unit fx, unit gy) of
    (Cotra _ s l, Cotra _ t r) ->
          (\v -> Day s t (\i j -> index (index v i) j))
      <$> traverse (\x -> traverse (\y -> f (xya x y)) r) l
