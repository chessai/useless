{-# language BangPatterns, DataKinds, KindSignatures, ScopedTypeVariables, TypeApplications, RoleAnnotations, GeneralizedNewtypeDeriving #-}

import GHC.TypeLits
import Data.Coerce
import Data.Proxy
import Control.Monad

newtype A (k :: Nat) (n :: Nat) a = A a
  deriving (Num, Floating, Fractional)

type role A nominal nominal representational

-- how we compute the estimate for the root
tactic :: forall k n a. (KnownNat k, KnownNat n, Floating a)
       => A k n a
       -> a
tactic !(A p) =
  let !n' = (fromIntegral $ natVal @n Proxy) :: a

  in (p / n')

-- compute the root
getRoot :: forall k n a. (KnownNat k, KnownNat n, Floating a)
        => A k n a
        -> a
getRoot principal@(A !p) =
  let !k' = (fromIntegral $ natVal @k Proxy) :: Int
      !n' = (fromIntegral $ natVal @n Proxy) :: a
      !t  = tactic principal 
      go !ix x_k = if (ix < k')
        then
          let !x_kp1 = (x_k / n') * (p / (x_k ** n') + n' - 1)
          in go (ix + 1) x_kp1
        else x_k
  in go 0 t

changeAcc :: A k n a
          -> A k' n a
changeAcc = coerce

changeRt :: A k n a
         -> A k n' a
changeRt = coerce

changeAccAndRt :: A k n a
               -> A k' n' a
changeAccAndRt = coerce

accept :: Double -- acceptable epsilon
       -> Double -- expected value
       -> Double -- actual value
       -> Bool   -- accept/reject
accept !eps !ev !av =
  let !err = (ev - av) / av
  in err <= eps 

main :: IO ()
main = do
  -- compute the square root (2) of val (a, b, etc.) over 5 iterations 
  let xs :: [A 10 2 Double]
      xs = coerce 
        ( [ 9
          , 256
          , 1000
          , 2390483920
          ] :: [Double]
        )
 
  let approx_Roots, actual_Roots :: [Double] 
      !approx_Roots = fmap getRoot xs
      !actual_Roots = fmap (coerce . sqrt) xs
      !eps = 0.00005
      !num_Acceptable = zipWith (accept eps) approx_Roots actual_Roots

  forM_ num_Acceptable print
