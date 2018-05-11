{-# language DeriveGeneric #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor  #-}
{-# language DeriveTraversable #-}

module Backwards where

import GHC.Generics (Generic, Generic1)
import Control.Applicative

-- | A functor with an 'Applicative' instance that performs
-- actions in reverse order.
newtype Backwards f a = Backwards { forwards :: f a }
  deriving (Foldable, Functor, Generic, Generic1, Traversable)

instance (Applicative f) => Applicative (Backwards f) where
  pure a = Backwards (pure a)
  Backwards f <*> Backwards a = Backwards (a <**> f)
