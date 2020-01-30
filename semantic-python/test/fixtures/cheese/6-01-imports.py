from cheese.ints import *

# EPath Import currentScope (Hole (cheese, ints))


# data Scope address = Scope
#   { edges        :: Map EdgeLabel [address]
#   , references   :: Map Reference ([ReferenceInfo], Path address)
#   , partialPaths :: Path address
#   , declarations :: Seq (Info address)
#   , domain       :: Domain


# data Path scope
#   = Hole (NonEmpty Name)
#   -- | Construct a direct path to a declaration.
#   | DPath Declaration Position
#   -- | Construct an edge from a scope to another declaration path.
#   | EPath EdgeLabel scope (Path scope)
#   deriving (Eq, Functor, Ord, Show)

# Scope <root>
#   references = Map (Reference one)
