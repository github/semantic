{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Stack.Node
  ( Info (..)
  , line_
  , span_
  , type_
  , kind_
  , info_
  , Node
  , Symbol
  , pattern Node
  , Type (..)

  ) where

import Analysis.Functor.Named
import Data.Functor.Tagged
import Data.Generics.Product
import Data.Text (Text)
import GHC.Generics (Generic)
import Source.Span hiding (line_)
import Tags.Tag (Kind (..))

-- | This type holds all relevant non-identificatory data about
-- a given 'Node'. The name and unique identifier are provided
-- by the 'Named' and 'Tagged' functors.
data Info = Info
  { line :: Text
  , kind :: Kind
  , span :: Span
  , typ' :: Type
  } deriving (Eq, Show, Generic)

line_ :: Lens' Info Text
line_ = field @"line"

kind_ :: Lens' Info Kind
kind_ = field @"kind"

instance HasSpan Info where
  span_ = field @"span"

type_ :: Lens' Info Type
type_ = field @"typ'"

-- | Nodes in a stack graph are tagged with a unique identifier and name information.
type Node = Tagged (Named Info)

info_ :: Lens' Node Info
info_ = contents.value_

pattern Node :: Tag -> Name -> Text -> Kind -> Span -> Type -> Node
pattern Node ident nam lin kin spa typ' = (Named nam (Info lin kin spa typ')) :# ident

type Symbol = Name

data Type
  = Root
  | JumpToScope
  | ExportedScope
  | InternalScope
  | Definition
  | Reference
  | Unknown
  | PushSymbol
  | PopSymbol
  | PushScope
  | IgnoreScope
  deriving (Show, Eq, Ord, Enum)

type Lens' s a = forall f . Functor f => (a -> f a) -> (s -> f s)
