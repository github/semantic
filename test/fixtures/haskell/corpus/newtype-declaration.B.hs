newtype O = O Int
newtype Show a => O = O a
newtype Karage = Karage { unKarage :: Maybe Int }
newtype Foo a (b :: [* -> *]) c = Bar (a b c)
newtype O = O Int deriving Show
newtype O = O a deriving (Eq, Ord, Enum, Bounded, Show, Read)
