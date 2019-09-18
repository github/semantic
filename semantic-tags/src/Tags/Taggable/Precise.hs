{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DeriveGeneric, DisambiguateRecordFields, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, OverloadedStrings, ScopedTypeVariables, TypeApplications, TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Tags.Taggable.Precise
( Python(..)
, runTagging
) where

import           Control.Effect.Reader
import           Data.Aeson as A
import           Data.Blob
import           Data.Monoid (Endo(..))
import           Data.Location
import qualified Data.Set as Set
import           Data.Text (Text)
import           GHC.Generics
import qualified TreeSitter.Python.AST as Python

data Tag = Tag
  { name :: Text
  , kind :: Text
  , span :: Span
  , context :: [Text]
  , line :: Maybe Text
  , docs :: Maybe Text
  }
  deriving (Eq, Generic, Show)

instance ToJSON Tag


data Kind
  = Function
  | Method
  | Class
  | Module
  | Call
  deriving (Bounded, Enum, Eq, Generic, Show)

instance ToJSON Kind where
  toJSON = toJSON . show
  toEncoding = toEncoding . show


newtype Python a = Python { getPython :: Python.Module a }
  deriving (Eq, Generic, Ord, Show)

type ContextToken = (Text, Maybe Range)

runTagging :: Blob -> Set.Set Text -> Python Location -> [Tag]
runTagging blob symbolsToSummarize
  = ($ [])
  . appEndo
  . run
  . runReader @[ContextToken] []
  . runReader blob
  . runReader symbolsToSummarize
  . tag
  . getPython where

class ToTag t where
  tag
    :: ( Carrier sig m
       , Member (Reader Blob) sig
       , Member (Reader [ContextToken]) sig
       , Member (Reader (Set.Set Text)) sig
       )
    => t Location
    -> m (Endo [Tag])

instance (ToTagBy strategy t, strategy ~ ToTagInstance t) => ToTag t where
  tag = tag' @strategy


class ToTagBy (strategy :: Strategy) t where
  tag'
    :: ( Carrier sig m
       , Member (Reader Blob) sig
       , Member (Reader [ContextToken]) sig
       , Member (Reader (Set.Set Text)) sig
       )
    => t Location
    -> m (Endo [Tag])


data Strategy = Generic | Custom

type family ToTagInstance t :: Strategy where
  ToTagInstance Python.FunctionDefinition = 'Custom
  ToTagInstance _                         = 'Generic

instance ToTagBy 'Custom Python.FunctionDefinition where
  tag' Python.FunctionDefinition {} = pure mempty


instance (Generic (t Location), GToTag (Rep (t Location))) => ToTagBy 'Generic t where
  tag' = gtag . from


class GToTag t where
  gtag
    :: ( Carrier sig m
       , Member (Reader Blob) sig
       , Member (Reader [ContextToken]) sig
       , Member (Reader (Set.Set Text)) sig
       )
    => t Location
    -> m (Endo [Tag])


instance GToTag (M1 i c f) where
  gtag _ = pure mempty
