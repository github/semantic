data Foo a b c where
  Baz :: a -> b -> c -> Foo a b c

data Foo f a where
  Bar :: { jolo :: Maybe String, runJolo :: f a } -> Foo f a

data Foo a :: [*] -> * where

data Number a where
  Integer :: !Prelude.Integer  -> Number Prelude.Integer
  Ratio   :: !Prelude.Rational -> Number Prelude.Rational
  Decimal :: !Scientific       -> Number Scientific

data Union (r :: [ * -> * ]) (v :: *) where
  Union :: {-# UNPACK #-} !Int -> t v -> Union r v
