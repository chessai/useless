-- credit: @mattoflambda
--
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

type family Let a where
  Let a = a

type family a =: b where
  a =: b = a ~ b

lmao :: Let d =: Double => d -> d -> d
lmao = (+)

