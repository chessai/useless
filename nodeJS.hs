{-# language OverloadedStrings #-}

import Data.String (IsString(..))

data Foo = Foo

instance IsString Foo where
  fromString _ = Foo

instance Num Foo where
  fromInteger _ = Foo

instance Eq Foo where
  _ == _ = True

default (Foo)

main :: IO ()
main = print $ 1 == "x"

{- 
$ ghci nodeJS.hs
GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( nodeJS.hs, interpreted )

nodeJS.hs:10:10: warning: [-Wmissing-methods]
    • No explicit implementation for
        ‘+’, ‘*’, ‘abs’, ‘signum’, and (either ‘negate’ or ‘-’)
    • In the instance declaration for ‘Num Foo’
   |
10 | instance Num Foo where
   |          ^^^^^^^
Ok, one module loaded.
*Main> main
True
*Main> :)
unknown command ':)'
use :? for help.
*Main> :q
Leaving GHCi.

-}
