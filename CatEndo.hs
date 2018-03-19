{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

import Control.Category
import Data.Monoid
import Data.Semigroup
import GHC.Generics (Generic)

-- | Category-polymorphic monoid of endomorphisms under composition.
newtype CatEndo c a
  = CatEndo { appCatEndo :: a `c` a }
  deriving (Generic)

instance (Category c) => Semigroup (CatEndo c a) where
  CatEndo f <> CatEndo g = CatEndo (f . g)

instance (Category c) => Monoid (CatEndo c a) where
  mempty = CatEndo id

