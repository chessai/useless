{-# language BangPatterns      #-}
{-# language DeriveGeneric     #-}
{-# language DeriveFoldable    #-}
{-# language DeriveFunctor     #-}
{-# language DeriveTraversable #-}
{-# language NoImplicitPrelude #-}

import           Data.Eq               (Eq((==)))
import           Data.Foldable         (Foldable)
import           Data.Functor          (Functor)
import           Data.Functor.Identity (Identity(..))
import           Data.Maybe            (Maybe(Just,Nothing))
import           Data.Ord              (Ord)
import           Data.Traversable      (Traversable)
import           GHC.Err               (error)
import           GHC.Generics          (Generic)
import           GHC.Show              (Show)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as DMS
import qualified Data.Map.Merge.Strict as Merge

-- | Encodes a diff between two 'as'.
data DeltaUnit a = DeltaUnit
  { old :: !a
  , new :: !a
  }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

-- | The result of a diff of an entry within two 'Map's.
data Delta a
  = Delta !(DeltaUnit a)
  | Old !a
  | New !a
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

-- | Takes two 'Map's and returns a 'Map' from the same key type to 'Delta' 'a',
--   where 'Delta' 'a' encodes differences between the keys.
--
--   The first 'Map' is treated as the /old/ 'Map', and any keys found in it but not
--   in the second, or /new/ 'Map', have their values put into 'Old'.
--   Keys found in the /new/ 'Map' but not in the first have their values put into 'New'.
--   Keys found in both 'Map's, but with different values, have their values stored in
--   'Delta', with the contained 'DeltaUnit' containing first the /old/ value and next
--   the /new/ value.
diff :: (Eq a, Ord k)
     => Map k a -- ^ first, /old/ 'Map'
     -> Map k a -- ^ second, /new/ 'Map'
     -> Map k (Delta a) -- ^ 'Map' encoding the diff
diff !m1 !m2 =
  Merge.merge
    (Merge.mapMissing (\_ x -> Old x)) -- preserve keys found in m1 but not m2
    (Merge.mapMissing (\_ x -> New x)) -- preserve keys found in m2 but not m1
    (Merge.zipWithMaybeMatched (\_ v1 v2 -> if v1 == v2 then Nothing else Just (Delta (DeltaUnit v1 v2))))
    m1
    m2
{-# INLINABLE diff #-}

getOld :: Delta a -> Maybe a
getOld (Delta (DeltaUnit a _)) = Just a
getOld (Old a)                 = Just a
getOld _                       = Nothing

getNew :: Delta a -> Maybe a
getNew (Delta (DeltaUnit _ a)) = Just a
getNew (New a)                 = Just a
getNew _                       = Nothing

getDelta :: Delta a -> Maybe (DeltaUnit a)
getDelta (Delta d) = Just d
getDelta _         = Nothing

-- | Retrieve only the old values out of the diff map.
toOld :: Map k (Delta a)
      -> Map k a
toOld = DMS.mapMaybe getOld

-- | Retrieve only the new values out of the diff map.
toNew :: Map k (Delta a)
      -> Map k a
toNew = DMS.mapMaybe getNew

toDelta :: Map k (Delta a)
        -> Map k (DeltaUnit a)
toDelta = DMS.mapMaybe getDelta
