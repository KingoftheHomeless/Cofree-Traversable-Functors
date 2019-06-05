{-# LANGUAGE DeriveFunctor, BangPatterns #-}
module Optimized where

import Control.Applicative
import Data.Traversable.Cofree
import Data.Traversable.Cofree.Optimized
import Data.Foldable (foldl')

-- | This newtype is associated with a semantically equivalent traversal as the one for
-- normal lists, but which uses '<*>' (or rather, 'liftA2')
-- in a left-associative manner rather than a right-associative manner.
newtype LeftAssoc a = LeftAssoc { unLeftAssoc :: [a] }
  deriving (Functor)

instance Foldable LeftAssoc where
  foldMap = foldMapDefault

-- | This instance is legal
instance Traversable LeftAssoc where
  traverse f (LeftAssoc l) = go (pure id) l
    where
      go !c [] = fmap (\cn -> LeftAssoc (cn [])) c
      go !c (x:xs) = go (liftA2 (\cn b -> cn . (b:)) c (f x)) xs

-- | More efficent implementation of sum
sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

-- | The naive implementation of the representational encoding is very inefficient
slowSum :: (Num a, Enum a) => a
slowSum = sum' . counit . unit $ [1..5000]

-- | The optimized implementation addresses this
quickSum :: (Num a, Enum a) => a
quickSum = sum' . counitOpt . unitOpt $ [1..5000]

-- | The optimized encoding still performs poorly with
-- left-associative uses of '<*>' in traversals
slowSum' :: (Num a, Enum a) => a
slowSum' = sum' . unLeftAssoc . counitOpt . unitOpt $ LeftAssoc [1..5000]

-- | This may be addressed by using 'unitOpt'' instead
quickSum' :: (Num a, Enum a) => a
quickSum' = sum' . unLeftAssoc . counitOpt . unitOpt' $ LeftAssoc [1..5000]
