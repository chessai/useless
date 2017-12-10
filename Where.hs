{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

type family Where a where
  Where a = a

type family a =: b where
  a =: b = a ~ b

lmao :: Where d =: Double => d -> d -> d
lmao = (+)

