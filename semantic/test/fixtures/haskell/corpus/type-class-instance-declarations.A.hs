instance Show Int where
instance Show Int where {}
instance Show Int
instance Show Int a where
instance Show Int a where {}
instance Show Int a
instance Show (Maybe a) where
instance Show (Maybe a) where {}
instance Show (Maybe a)
instance Show (a, b, c) where
instance Show (a, b, c) where {}
instance Show (a, b, c)
instance Show [a] where
instance Show [a] where {}
instance Show [a]
instance Show (a -> b) where
instance Show (a -> b) where {}
instance Show (a -> b)
instance Show Foo where
  bar (Foo Baz) (Foo Wix) = EQ
instance Show Foo where {
  bar (Foo Baz) (Foo Wix) = EQ
}
instance Show (,) where
instance Show (,) where {}
instance Show (,)
instance Show (Bar, Baz a b) where
instance Show (Bar, Baz a b) where {}
instance Show (Bar, Baz a b)
instance Show [(Bar, Baz a b)] where
instance Show [(Bar, Baz a b)] where {}
instance Show [(Bar, Baz a b)]
instance Show [Bar] where
instance Show [Bar] where {}
instance Show [Bar]
instance Show [Bar a b] where
instance Show [Bar a b] where {}
instance Show [Bar a b]
instance Show [Bar Baz b] where
instance Show [Bar Baz b] where {}
instance Show [Bar Baz b]

instance Show a => Read Int where {}
instance Show a => Read (Maybe a) where {}
instance (Show a, Eq a) => Read (Maybe a) where {}
instance (Foo (Bar [Baz])) => Read (Bar) where {}
instance (Foo (Bar (Baz, Baz))) => Read (Bar) where {}

instance Foo Bar where
  baz :: Num b => a -> b -> a
  baz' :: (Num a, Num b) => Maybe a -> Either String b -> Maybe (Either String a)

instance Bar a b m => Baz a b m where {}

instance ( Foo (Bar a b '[]) c ) => Baz a b (Bix a b c) where

instance (Bar baz ~ foo) => Wix baz where
  toWix = undefined
  Wix baz <> Wix baz' = Wix (baz <> baz')

instance Bar (f :+: g) where {}

instance (A :< b, B :< b) => Bar (A c) Foo where {}

instance Foo Bar where
  foo D.F{..} = foo

instance Show Foo where
  bar (Foo Baz) (Foo Baz) = EQ

instance forall location a b. (Show a, Monad b) => MonadError (Value a) b where
  unit = pure . Unit

instance Show A where b = c
-- a
instance Show A where b = c
