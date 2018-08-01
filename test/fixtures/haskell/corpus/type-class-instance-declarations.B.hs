instance Eq Int where
instance Eq Int where {}
instance Eq Int
instance Eq Int a where
instance Eq Int a where {}
instance Eq Int a
instance Eq (Maybe a) where
instance Eq (Maybe a) where {}
instance Eq (Maybe a)
instance Eq (a, b, c) where
instance Eq (a, b, c) where {}
instance Eq (a, b, c)
instance Eq [a] where
instance Eq [a] where {}
instance Eq [a]
instance Eq (a -> b) where
instance Eq (a -> b) where {}
instance Eq (a -> b)
instance Eq Foo where
  bar (Foo Baz) (Foo Wix) = EQ
instance Eq Foo where {
  bar (Foo Baz) (Foo Wix) = EQ
}
instance Eq (,) where
instance Eq (,) where {}
instance Eq (,)
instance Eq (Bar, Baz a b) where
instance Eq (Bar, Baz a b) where {}
instance Eq (Bar, Baz a b)
instance Eq [(Bar, Baz a b)] where
instance Eq [(Bar, Baz a b)] where {}
instance Eq [(Bar, Baz a b)]
instance Eq [Bar] where
instance Eq [Bar] where {}
instance Eq [Bar]
instance Eq [Bar a b] where
instance Eq [Bar a b] where {}
instance Eq [Bar a b]
instance Eq [Bar Baz b] where
instance Eq [Bar Baz b] where {}
instance Eq [Bar Baz b]

instance Show a => Read Int where {}
instance Show a => Read (Maybe a) where {}
instance (Show a, Eq a) => Read (Maybe a) where {}
instance (Foo (Bar [Baz])) => Read (Bar) where {}
instance (Foo (Bar (Baz, Baz))) => Read (Bar) where {}

instance Bar Foo where
  baz :: Num b => a -> b -> a
  baz' :: (Num a, Num b) => Maybe a -> Either String b -> Maybe (Either String a)

instance Foo a b m => Bar a b m where {}

instance ( Foo (Bar a b '[]) c ) => Baz a b (Bix a b c) where

instance (Foo baz ~ bar) => Wix baz where
  toWix = undefined
  Wix baz <> Wix baz' = Wix (baz <> baz')

instance Foo (f :+: g) where {}

instance (B :< a, A :< b) => Foo (A b) Bar where {}

instance Bar Foo where
  foo F.D{..} = bar

instance Show Bar where
  bar (Bar Baz) (Bar Baz) = EQ

instance forall location b c. (Show b, Monad c) => ErrorMonad (Value b) c where
  unit = pure . Unit

instance Show B where c = d
-- a
instance Show B where c = d
