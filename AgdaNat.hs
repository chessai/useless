{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

import Data.Kind          (Type)
import Data.Coerce        (coerce)
import Numeric.Natural    (Natural)
import Data.Type.Equality ((:~:)(..),gcastWith)
import Unsafe.Coerce      (unsafeCoerce)

data family The k :: k -> Type

class Sing (a :: k) where sing :: The k (a :: k)

data Nat = Z | S Nat

newtype instance The Nat n = NatSing Natural

instance Sing Z where
    sing = NatSing 0

instance Sing n => Sing (S n) where
    sing =
        (coerce :: (Natural -> Natural) -> (The Nat n -> The Nat (S n)))
            succ sing

data Natty n where
        ZZy :: Natty Z
        SSy :: The Nat n -> Natty (S n)

getNatty :: The Nat n -> Natty n
getNatty (NatSing n :: The Nat n) = case n of
  0 -> gcastWith (unsafeCoerce Refl :: n :~: Z) ZZy
  _ -> gcastWith (unsafeCoerce Refl :: n :~: S m) (SSy (NatSing (pred n)))

pattern Zy :: () => (n ~ Z) => The Nat n
pattern Zy <- (getNatty -> ZZy) where Zy = NatSing 0

pattern Sy :: () => (n ~ S m) => The Nat m -> The Nat n
pattern Sy x <- (getNatty -> SSy x) where Sy (NatSing x) = NatSing (succ x)
{-# COMPLETE Zy, Sy #-}

type family (+) (n :: Nat) (m :: Nat) :: Nat where
        Z + m = m
        S n + m = S (n + m)

-- | Efficient addition, with type-level proof.
add :: The Nat n -> The Nat m -> The Nat (n + m)
add = (coerce :: (Natural -> Natural -> Natural) -> The Nat n -> The Nat m -> The Nat (n + m)) (+)

-- | Proof on efficient representation.
addZeroRight :: The Nat n -> n + Z :~: n
addZeroRight Zy = Refl
addZeroRight (Sy n) = gcastWith (addZeroRight n) Refl
