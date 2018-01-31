{-# language OverloadedStrings #-}

import Data.String (IsString(..))

data Foo = Foo

instance IsString Foo where
  fromString _ = Foo

instance Num Foo where
  fromInteger _ = Foo
  {-# INLINE fromInteger #-} 
  _ + _ = Foo
  {-# INLINE (+) #-} 
  _ * _ = Foo
  {-# INLINE (*) #-} 
  _ - _ = Foo
  {-# INLINE (-) #-} 
  abs _ = Foo
  {-# INLINE abs #-} 
  signum _ = 1
  {-# INLINE signum #-}

instance Eq Foo where
  _ == _ = True

default (Foo)

main :: IO ()
main = print $ 1 == "1"

{-
$ ghci nodeJS.hs 
GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( nodeJS.hs, interpreted )
Ok, one module loaded.
*Main> main
True
-}
