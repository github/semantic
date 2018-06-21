class Eq a where {}
class Ord a b where {}

class Eq a => Ord a where {}
class (Show a, Eq a) => Ord a where {}

class Bar a where {
  op' :: Num b => a -> b -> a;
  op :: (Num a, Num b) => a -> b -> a;
}

class Bar a where
  infixl `op`
  infixr 8 `op`
  infix 8 `op`, `ip`, `ap`
  infix <$>
  infix 8 <$>
  infix 8 :
  infix 8 :.
  infix 8 :<:

class (Eq b) => Ord b where
  compare              :: b -> b -> Ordering
  (<), (<=), (>=), (>) :: b -> b -> Bool
  max, min             :: b -> b -> b
  id                   :: b

class Foo a b m => Bar a b m where {}

class Foo bat where
  bar :: bat -> Baz
  default bar :: bat -> Baz

class Foo (baz :: Bar) where

class Effectful' (m' :: [* -> *] -> * -> *) where

class Foo bar where
  type Baz wiz :: Wax

class Foo bar where
  type Baz wiz :: [* -> *]

class Foo baz => Bar fax where
  type family Woot a :: [* -> *]

class (Monad a, Show b) => Foo a b c | b -> d, a -> b where
  d :: a b
