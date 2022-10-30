{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module MyLib where

data Z

data S n

data SizedList a n where
  Empty :: SizedList a Z
  Cons :: a -> (SizedList a n) -> SizedList a (S n)

(++) :: SizedList a n -> SizedList a m -> SizedList a (Sum n m)
Empty ++ arr2 = arr2
(Cons x xs) ++ arr2 = Cons x $ xs MyLib.++ arr2

remove :: SizedList a (S n) -> SizedList a n
remove (Cons x xs) = xs

head :: SizedList a (S n) -> a
head (Cons x _) = x

tail :: SizedList a (S n) -> SizedList a n
tail (Cons x xs) = xs

instance (Show a) => Show (SizedList a n) where
  show Empty = ""
  show (Cons x xs) = show x Prelude.++ ", " Prelude.++ show xs

type family Sum a b where
  Sum Z b = b
  Sum (S a) b = S (Sum a b)
