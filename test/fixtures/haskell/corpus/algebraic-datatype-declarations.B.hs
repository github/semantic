data O
data O a = O a
data O a = O !a
data O a b = O !a b

data A = B
       | B0
       | B9
       | Ba
       | B_
       | Bz'

data O = O { a :: Int }
data O = O { a, b :: Int }
data O = O { a :: !Int, b :: Int }
data O = O { a, b :: {-#  UNPACK #-} !Int, c :: String }
data N = N { b :: Int } | O { c :: String }
data N = N { b :: Text } | O { c :: Bool }

data N = N deriving Show
data N = N deriving (Functor, Ord, Enum, Bounded, Show, Read)

data Monad a => N a = N a
data (Ord a, Show a, Eq b) => N a b = N a b
data (Eq (f a), Applicative f) => N f a = N f a

data Foo bar = HasCallStack => Wiz bar
data Baz a = Show a => Baz a


data Bar = Bar !Double#

data SomeNumber = forall b . Show b => SomeNumber (Number b)
