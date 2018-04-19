{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

import Data.Kind (Type)

data Nat = Z | S Nat

data Vec :: Nat -> Type -> Type where
  Nil  :: Vec Z a
  Cons :: a -> Vec n a -> Vec (S n) a

data Matrix :: [Nat] -> Type -> Type where
  NilM  :: a -> Matrix '[] a
  ConsM :: Vec n (Matrix ns a) -> Matrix (n ': ns) a
