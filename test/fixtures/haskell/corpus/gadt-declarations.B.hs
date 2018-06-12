data Bar a b c where
  Baz :: a -> b -> c -> Bar a b c

data Bar f a where
  Baz :: { jolo :: Maybe String, runJolo :: f a } -> Bar f a

data Bar a :: [*] -> [*] where

data Number' a where
  Integer' :: !Prelude.Integer  -> Number Prelude.Integer
  Ratio'   :: !Prelude.Rational -> Number Prelude.Rational
  Decimal' :: !Scientific       -> Number Scientific

data Union (r :: [ * -> * ]) (v :: *) where
  Union :: {-# UNPACK #-} !Integer -> t v -> Union r v
