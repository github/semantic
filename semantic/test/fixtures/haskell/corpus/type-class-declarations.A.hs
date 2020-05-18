class Show a where {}
class Show a b where {}

class Show a => Read a where {}
class (Show a, Eq a) => Read a where {}

class Foo a where {
  op :: Num b => a -> b -> a;
  op' :: (Num a, Num b) => a -> b -> a;
}

class Foo a where
  infixl `op`
  infixr 7 `op`
  infix 7 `op`, `ip`, `ap`
  infix <$>
  infix 7 <$>
  infix 7 :
  infix 7 :.
  infix 7 :<:

class (Eq a) => Ord a where
  compare              :: a -> a -> Ordering
  (<), (<=), (>=), (>) :: a -> a -> Bool
  max, min             :: a -> a -> a
  id                   :: a

class Bar a b m => Baz a b m where {}

class Bar baz where
  foo :: wiz -> Baz
  default foo :: wiz -> Baz

class Bar (baz :: Foo) where

class Effectful (m :: [* -> *] -> * -> *) where

class Foo bar where
  type Baz wiz :: Wax

class Foo bar where
  type Baz wiz :: [* -> *]

class Bar baz => Foo fax where
  type family Woo a :: [* -> *]

class (Monad a, Show b) => Foo a b c | a -> c, b -> c where
  d :: a b
