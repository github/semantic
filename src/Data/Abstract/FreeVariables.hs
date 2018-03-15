{-# LANGUAGE DefaultSignatures, UndecidableInstances #-}
module Data.Abstract.FreeVariables where

import Prologue
import Data.Term
import Data.ByteString (intercalate)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B (filter)
import Data.Char (ord)

-- | The type of variable names.
type Name = NonEmpty ByteString

-- | Construct a qualified name from a 'ByteString'
name :: ByteString -> Name
name x = x :| []

-- | Construct a qualified name from a list of 'ByteString's
qualifiedName :: [ByteString] -> Name
qualifiedName = NonEmpty.fromList

-- | Construct a qualified 'Name' from a `/` delimited path.
pathToQualifiedName :: ByteString -> Name
pathToQualifiedName = qualifiedName . splitOnPathSeparator

-- | Split a 'ByteString' path on `/`, stripping quotes and any `./` prefix.
splitOnPathSeparator :: ByteString -> [ByteString]
splitOnPathSeparator = BC.split '/' . BC.dropWhile (== '/') . BC.dropWhile (== '.') . stripQuotes
  where stripQuotes = B.filter (/= fromIntegral (ord '\"'))

-- | User friendly 'ByteString' of a qualified 'Name'.
friendlyName :: Name -> ByteString
friendlyName xs = intercalate "." (NonEmpty.toList xs)


-- | Types which can contain unbound variables.
class FreeVariables term where
  -- | The set of free variables in the given value.
  freeVariables :: term -> Set Name


-- | A lifting of 'FreeVariables' to type constructors of kind @* -> *@.
--
--   'Foldable' types requiring no additional semantics to the set of free variables (e.g. types which do not bind any variables) can use (and even derive, with @-XDeriveAnyClass@) the default implementation.
class FreeVariables1 syntax where
  -- | Lift a function mapping each element to its set of free variables through a containing structure, collecting the results into a single set.
  liftFreeVariables :: (a -> Set Name) -> syntax a -> Set Name
  default liftFreeVariables :: (Foldable syntax) => (a -> Set Name) -> syntax a -> Set Name
  liftFreeVariables = foldMap

-- | Lift the 'freeVariables' method through a containing structure.
freeVariables1 :: (FreeVariables1 t, FreeVariables a) => t a -> Set Name
freeVariables1 = liftFreeVariables freeVariables

freeVariable :: FreeVariables term => term -> Name
freeVariable term = case toList (freeVariables term) of
  [n] -> n
  xs -> Prelude.fail ("expected single free variable, but got: " <> show xs)

-- TODO: Need a dedicated concept of qualified names outside of freevariables (a
-- Set) b/c you can have something like `a.a.b.a`
-- qualifiedName :: FreeVariables term => term -> Name
-- qualifiedName term = let names = toList (freeVariables term) in B.intercalate "." names

instance (FreeVariables1 syntax, Functor syntax) => FreeVariables (Term syntax ann) where
  freeVariables = cata (liftFreeVariables id)

instance (FreeVariables1 syntax) => FreeVariables1 (TermF syntax ann) where
  liftFreeVariables f (In _ s) = liftFreeVariables f s

instance (Apply FreeVariables1 fs) => FreeVariables1 (Union fs) where
  liftFreeVariables f = apply (Proxy :: Proxy FreeVariables1) (liftFreeVariables f)

instance FreeVariables1 []
