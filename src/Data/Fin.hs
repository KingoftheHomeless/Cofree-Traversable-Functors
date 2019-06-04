{-# LANGUAGE DataKinds, EmptyCase, GADTs #-}
module Data.Fin where

import Data.Nat

data Fin n where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

absurdFin :: Fin 'Z -> a
absurdFin x = case x of

toFin :: SNat n -> Fin ('S n)
toFin SZ     = FZ
toFin (SS n) = FS (toFin n)

raise :: Fin n -> Fin ('S n)
raise FZ      = FZ
raise (FS fn) = FS (raise fn)
