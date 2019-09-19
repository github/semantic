{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DeriveGeneric, DisambiguateRecordFields, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, OverloadedStrings, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Tags.Taggable.Precise
( runTagging
) where

import           Control.Effect.Reader
import           Control.Effect.Writer
import           Data.Aeson as A
import           Data.Foldable (traverse_)
import           Data.Monoid (Endo(..))
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Location
import           Data.Source
import           Data.Text as T
import           GHC.Generics
import qualified TreeSitter.Python.AST as Python

data Tag = Tag
  { name :: Text
  , kind :: Kind
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


type ContextToken = (Text, Maybe Range)

runTagging :: Source -> Python.Module Location -> [Tag]
runTagging source
  = ($ [])
  . appEndo
  . run
  . execWriter
  . runReader @[ContextToken] []
  . runReader source
  . tag where

class ToTag t where
  tag
    :: ( Carrier sig m
       , Member (Reader Source) sig
       , Member (Reader [ContextToken]) sig
       , Member (Writer (Endo [Tag])) sig
       )
    => t
    -> m ()

instance (ToTagBy strategy t, strategy ~ ToTagInstance t) => ToTag t where
  tag = tag' @strategy


class ToTagBy (strategy :: Strategy) t where
  tag'
    :: ( Carrier sig m
       , Member (Reader Source) sig
       , Member (Reader [ContextToken]) sig
       , Member (Writer (Endo [Tag])) sig
       )
    => t
    -> m ()


data Strategy = Generic | Custom

type family ToTagInstance t :: Strategy where
  ToTagInstance Location                             = 'Custom
  ToTagInstance Text                                 = 'Custom
  ToTagInstance [_]                                  = 'Custom
  ToTagInstance (Either _ _)                         = 'Custom
  ToTagInstance (Python.FunctionDefinition Location) = 'Custom
  ToTagInstance _                                    = 'Generic

instance ToTagBy 'Custom Location where
  tag' _ = pure ()

instance ToTagBy 'Custom Text where
  tag' _ = pure ()

instance ToTag t => ToTagBy 'Custom [t] where
  tag' = traverse_ tag

instance (ToTag l, ToTag r) => ToTagBy 'Custom (Either l r) where
  tag' = either tag tag

instance ToTagBy 'Custom (Python.FunctionDefinition Location) where
  tag' Python.FunctionDefinition
    { ann = Location Range { start } span
    , name = Python.Identifier { bytes = name }
    , parameters
    , returnType
    , body = Python.Block { ann = Location Range { start = end } _, extraChildren }
    } = do
      src <- ask @Source
      let docs = case extraChildren of
            x:_ | Just (Python.String { ann }) <- docComment x -> Just (toText (slice (locationByteRange ann) src))
            _                                                  -> Nothing
          sliced = slice (Range start end) src
      tell (Endo (Tag name Function span [] (Just (firstLine sliced)) docs :))
      tag parameters
      tag returnType
      traverse_ tag extraChildren

docComment :: Either (Python.CompoundStatement a) (Python.SimpleStatement a) -> Maybe (Python.String a)
docComment (Right (Python.ExpressionStatementSimpleStatement (Python.ExpressionStatement { extraChildren = Left (Python.PrimaryExpressionExpression (Python.StringPrimaryExpression s)) :|_ }))) = Just s
docComment _ = Nothing

firstLine :: Source -> Text
firstLine = T.take 180 . T.takeWhile (/= '\n') . toText

instance (Generic t, GToTag (Rep t)) => ToTagBy 'Generic t where
  tag' = gtag . from


class GToTag t where
  gtag
    :: ( Carrier sig m
       , Member (Reader Source) sig
       , Member (Reader [ContextToken]) sig
       , Member (Writer (Endo [Tag])) sig
       )
    => t a
    -> m ()


instance GToTag f => GToTag (M1 i c f) where
  gtag = gtag . unM1

instance (GToTag f, GToTag g) => GToTag (f :*: g) where
  gtag (f :*: g) = (<>) <$> gtag f <*> gtag g

instance (GToTag f, GToTag g) => GToTag (f :+: g) where
  gtag (L1 l) = gtag l
  gtag (R1 r) = gtag r

instance ToTag t => GToTag (K1 R t) where
  gtag = tag . unK1

instance GToTag U1 where
  gtag _ = pure mempty
