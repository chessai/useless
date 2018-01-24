{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

type family Let a where
  Let a = a

lmao :: Let d ~ Double => d -> d -> d
lmao = (+)

lmaoInt :: Let i ~ Int => i -> i -> i
lmaoInt = (+)
