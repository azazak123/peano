{-# LANGUAGE TypeFamilies #-}

module Data.Peano where

data Z

data S n

type family Sum a b where
  Sum Z b = b
  Sum (S a) b = S (Sum a b)
