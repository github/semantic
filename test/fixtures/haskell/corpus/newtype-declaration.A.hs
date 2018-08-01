newtype N = N Int
newtype Show a => N = N a
newtype Age = Age { unAge :: Maybe Int }
newtype Bar a (b :: [* -> *]) c = Foo (a b c)
newtype N = N Int deriving Show
newtype N = N a deriving (Eq, Ord, Enum, Bounded, Show, Read)
