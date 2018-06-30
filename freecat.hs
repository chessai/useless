{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

import Data.Kind (Type)

type Graph node = node -> node -> Type

data Node = Employee | Department | Str

data Work :: Graph Node where
  Manager :: Work Employee Employee
  WorksIn :: Work Employee Department
  Name    :: Work Employee Str
  DepName :: Work Department Str

data Empl = Empl
  { name :: String
  , manager :: Empl
  , dep :: Dep
  }

data Dep = Dep
  { depName :: String }

type family
  El (node :: Node) = (res :: Type) | res -> node where
  El Employee   = Empl
  El Department = Dep
  El Str        = String

varp :: Work a b -> (El a -> El b)
varp = \case
  Manager -> manager
  WorksIn -> dep
  Name    -> name
  DepName -> depName

