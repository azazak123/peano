{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Peano where

data Z

data S n

type family Add a b where
  Add Z b = b
  Add (S a) b = S (Add a b)

type family Mul a b where
  Mul (S Z) b = b
  Mul (S a) b = Add (Mul a b) b
