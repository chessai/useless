import Prelude hiding ((&&), (||), not)
import qualified Prelude

class Heyting a where
  ff      :: a
  tt      :: a
  implies :: a -> a -> a
  conj    :: a -> a -> a
  disj    :: a -> a -> a
  not     :: a -> a

infixr 3 &&
infixr 2 ||

(&&) :: Heyting a => a -> a -> a
(&&) = conj
(||) :: Heyting a => a -> a -> a
(||) = disj

instance Heyting Bool where
  ff = False
  tt = True
  implies a b = not a || b
  conj True True = True
  conj _    _    = False
  disj True _    = True
  disj _    True = True
  disj _    _    = False
  not  True      = False
  not  _         = True

(??) :: a -> a -> Bool -> a
a ?? b = \x -> if x then a else b

andM :: Monad m => m Bool -> m Bool -> m Bool
andM a b = a >>= \x -> (b ?? pure x) x

orM :: Monad m => m Bool -> m Bool -> m Bool
orM a b = a >>= \x -> (pure x ?? b) x

scFalse :: (Foldable t, Monad m) => t (m Bool) -> m Bool
scFalse = foldr andM (pure False)

scTrue :: (Foldable t, Monad m) => t (m Bool) -> m Bool
scTrue = foldr orM (pure True)

foo
  :: (Functor t, Foldable t, Monad m)
  => t (a -> b -> m Bool)  -- ^ conditions
  -> (a, b)                -- ^ inputs
  -> m Bool
foo cs is = scFalse $ fmap (($ is) . uncurry) cs
