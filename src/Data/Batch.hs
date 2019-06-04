module Data.Batch where

data Batch a b c = P c | Batch a b (b -> c) :*: a
infixl 1 :*: -- :*: infixes to the left

instance Functor (Batch a b) where
  fmap f (P c) = P (f c)
  fmap f (r :*: a) = fmap (f .) r :*: a

instance Applicative (Batch a b) where
  pure = P
  u <*> P a = fmap ($ a) u
  u <*> (v :*: a) = ((.) <$> u <*> v) :*: a

batch :: a -> Batch a b b
batch a = P id :*: a

build :: Traversable t => t a -> Batch a b (t b)
build = traverse batch
