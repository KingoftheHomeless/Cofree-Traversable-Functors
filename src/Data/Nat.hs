{-# LANGUAGE DataKinds, GADTs #-}
module Data.Nat where

data Nat
  = Z
  | S Nat

data SNat n where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)
