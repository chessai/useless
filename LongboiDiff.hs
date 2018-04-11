{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Category
import Prelude (undefined)

data Nat = Z | S Nat 

data Vec (n :: Nat) a where
  Nil :: Vec Z a
  Cons :: a -> Vec n a -> Vec (S n) a

--append :: Vec n a -> Vec m a -> Vec (n + m) a
--append (Cons x xs) ys = Cons x (append xs ys)
--append Nil ys = ys

empty :: Vec Z a
empty = Nil

type family (n :: Nat) + (m :: Nat) where
  Z + m = m
  n + Z = n
  (S n) + m = S (n + m)

newtype D a n m = D { runD :: Vec n a -> Vec m a }

toD :: Vec n a -> D a m (m + n)
toD v = undefined

instance Category (D a) where
  id :: D a n n
  id = toD empty
  (.) :: D a m o -> D a n m -> D a n o
  xs . ys = D (runD xs . runD ys)
