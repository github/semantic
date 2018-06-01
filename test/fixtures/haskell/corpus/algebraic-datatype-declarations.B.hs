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
