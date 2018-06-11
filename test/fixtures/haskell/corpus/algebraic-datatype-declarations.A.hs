data N
data N a = N a
data N a = N !a
data N a b = N !a b

data B = A
       | A0
       | A9
       | Aa
       | A_
       | Az'

data N = N { a :: Int }
data N = N { a, b :: Int }
data N = N { a :: !Int, b :: Int }
data N = N { a, b :: {-#  UNPACK #-} !Int, c :: String }
data N = N { a :: Int } | O { b :: String }
data N = N { b :: Int } | O { c :: String }

data N = N deriving Show
data N = N deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Show a => N a = N a
data (Eq a, Show a, Eq b) => N a b = N a b
data (Eq (f a), Functor f) => N f a = N f a

data Foo bar = HasCallStack => Foo bar
data Baz foo = Show foo => Baz foo

data Foo = Foo !Double#

data SomeNumber = forall a . Show a => SomeNumber (Number a)
