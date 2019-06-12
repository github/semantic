{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, OverloadedLists, OverloadedStrings,StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Data.Name
( User
, Namespaced
, Name(..)
, reservedNames
, isSimpleCharacter
, needsQuotation
, Gensym(..)
, (//)
, gensym
, namespace
, Naming(..)
, runNaming
, NamingC(..)
) where

import           Control.Applicative
import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Effect.Sum
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import qualified Data.Char as Char
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Text as Text (Text, any, unpack)
import           Data.Text.Prettyprint.Doc (Pretty (..))
import qualified Data.Text.Prettyprint.Doc as Pretty

-- | User-specified and -relevant names.
type User = Text

-- | The type of namespaced actions, i.e. actions occurring within some outer name.
--
--   This corresponds to the @Agent@ type synonym described in /I Am Not a Number—I Am a Free Variable/.
type Namespaced a = Gensym -> a

data Name
  -- | A locally-bound, machine-generatable name.
  --
  --   This should be used for locals, function parameters, and similar names which can’t escape their defining scope.
  = Gen Gensym
  -- | A name provided by a user.
  --
  --   This should be used for names which the user provided and which other code (other functions, other modules, other packages) could call, e.g. declaration names.
  | User User
  -- | A variable name represented as the path to a source file. Used for loading modules at a specific name.
  | Path Text
  deriving (Eq, Ord, Show)

instance Pretty Name where
  pretty = \case
    Gen p  -> pretty p
    User n -> pretty n
    Path p -> pretty (show p)

reservedNames :: HashSet String
reservedNames = [ "#true", "#false", "let", "#frame", "if", "then", "else"
                , "lexical", "import", "#unit", "load"]

-- | Returns true if any character would require quotation or if the
-- name conflicts with a Core primitive.
needsQuotation :: User -> Bool
needsQuotation u = HashSet.member (unpack u) reservedNames || Text.any (not . isSimpleCharacter) u

-- | A ‘simple’ character is, loosely defined, a character that is compatible
-- with identifiers in most ASCII-oriented programming languages. This is defined
-- as the alphanumeric set plus @$@ and @_@.
isSimpleCharacter :: Char -> Bool
isSimpleCharacter = \case
  '$'  -> True -- common in JS
  '_'  -> True
  '?'  -> True -- common in Ruby
  c    -> Char.isAlphaNum c

data Gensym
  = Root Text
  | Gensym :/ (Text, Int)
  deriving (Eq, Ord, Show)

instance Pretty Gensym where
  pretty = \case
    Root s      -> pretty s
    p :/ (n, x) -> Pretty.hcat [pretty p, "/", pretty n, "^", pretty x]

(//) :: Gensym -> Text -> Gensym
root // s = root :/ (s, 0)

infixl 6 //

gensym :: (Carrier sig m, Member Naming sig) => Text -> m Gensym
gensym s = send (Gensym s pure)

namespace :: (Carrier sig m, Member Naming sig) => Text -> m a -> m a
namespace s m = send (Namespace s m pure)


data Naming m k
  = Gensym Text (Gensym -> k)
  | forall a . Namespace Text (m a) (a -> k)

deriving instance Functor (Naming m)

instance HFunctor Naming where
  hmap _ (Gensym    s   k) = Gensym    s       k
  hmap f (Namespace s m k) = Namespace s (f m) k

instance Effect Naming where
  handle state handler (Gensym    s   k) = Gensym    s                        (handler . (<$ state) . k)
  handle state handler (Namespace s m k) = Namespace s (handler (m <$ state)) (handler . fmap k)


runNaming :: Functor m => Gensym -> NamingC m a -> m a
runNaming root = runReader root . evalState 0 . runNamingC

newtype NamingC m a = NamingC { runNamingC :: StateC Int (ReaderC Gensym m) a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadIO)

instance (Carrier sig m, Effect sig) => Carrier (Naming :+: sig) (NamingC m) where
  eff (L (Gensym    s   k)) = NamingC (StateC (\ i -> (:/ (s, i)) <$> ask >>= runState (succ i) . runNamingC . k))
  eff (L (Namespace s m k)) = NamingC (StateC (\ i -> local (// s) (evalState 0 (runNamingC m)) >>= runState i . runNamingC . k))
  eff (R other)             = NamingC (eff (R (R (handleCoercible other))))
