{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DisambiguateRecordFields, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Language.Python.Tags
( Term(..)
) where

import           Control.Effect.Reader
import           Control.Effect.Writer
import           Data.Foldable (traverse_)
import           Data.Maybe (listToMaybe)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text as T
import           GHC.Generics
import           Source.Loc
import           Source.Range
import           Source.Source
import           Tags.Tag
import qualified Tags.Taggable.Precise as Tags
import qualified TreeSitter.Python.AST as Py

class ToTag t where
  tags
    :: ( Carrier sig m
       , Member (Reader Source) sig
       , Member (Writer Tags.Tags) sig
       )
    => t Loc
    -> m ()

newtype Term a = Term { getTerm :: Py.Module a }

instance Tags.ToTag Term where
  tags = tags . getTerm


instance (ToTagBy strategy t, strategy ~ ToTagInstance t) => ToTag t where
  tags = tags' @strategy


class ToTagBy (strategy :: Strategy) t where
  tags'
    :: ( Carrier sig m
        , Member (Reader Source) sig
        , Member (Writer Tags.Tags) sig
        )
    => t Loc
    -> m ()


data Strategy = Generic | Custom

type family ToTagInstance t :: Strategy where
  ToTagInstance (_ :+: _)             = 'Custom
  ToTagInstance Py.FunctionDefinition = 'Custom
  ToTagInstance Py.ClassDefinition    = 'Custom
  ToTagInstance Py.Call               = 'Custom
  ToTagInstance _                     = 'Generic

instance (ToTag l, ToTag r) => ToTagBy 'Custom (l :+: r) where
  tags' (L1 l) = tags l
  tags' (R1 r) = tags r

instance ToTagBy 'Custom Py.FunctionDefinition where
  tags' Py.FunctionDefinition
    { ann = Loc Range { start } span
    , name = Py.Identifier { bytes = name }
    , parameters
    , returnType
    , body = Py.Block { ann = Loc Range { start = end } _, extraChildren }
    } = do
      src <- ask @Source
      let docs = listToMaybe extraChildren >>= docComment src
          sliced = slice src (Range start end)
      Tags.yield (Tag name Function span (firstLine sliced) docs)
      tags parameters
      traverse_ tags returnType
      traverse_ tags extraChildren

instance ToTagBy 'Custom Py.ClassDefinition where
  tags' Py.ClassDefinition
    { ann = Loc Range { start } span
    , name = Py.Identifier { bytes = name }
    , superclasses
    , body = Py.Block { ann = Loc Range { start = end } _, extraChildren }
    } = do
      src <- ask @Source
      let docs = listToMaybe extraChildren >>= docComment src
          sliced = slice src (Range start end)
      Tags.yield (Tag name Class span (firstLine sliced) docs)
      traverse_ tags superclasses
      traverse_ tags extraChildren

instance ToTagBy 'Custom Py.Call where
  tags' Py.Call
    { ann = Loc range span
    , function = Py.IdentifierPrimaryExpression Py.Identifier { bytes = name }
    , arguments
    } = do
      src <- ask @Source
      let sliced = slice src range
      Tags.yield (Tag name Call span (firstLine sliced) Nothing)
      tags arguments
  tags' Py.Call {} = pure ()

docComment :: Source -> (Py.CompoundStatement :+: Py.SimpleStatement) Loc -> Maybe Text
docComment src (R1 (Py.ExpressionStatementSimpleStatement (Py.ExpressionStatement { extraChildren = L1 (Py.PrimaryExpressionExpression (Py.StringPrimaryExpression Py.String { ann })) :|_ }))) = Just (toText (slice src (byteRange ann)))
docComment _ _ = Nothing

firstLine :: Source -> Text
firstLine = T.take 180 . T.takeWhile (/= '\n') . toText

instance (Generic1 t, GToTag (Rep1 t)) => ToTagBy 'Generic t where
  tags' = gtags . from1

class GToTag t where
  gtags
    :: ( Carrier sig m
        , Member (Reader Source) sig
        , Member (Writer Tags.Tags) sig
        )
    => t Loc
    -> m ()


instance GToTag f => GToTag (M1 i c f) where
  gtags = gtags . unM1

instance (GToTag f, GToTag g) => GToTag (f :*: g) where
  gtags (f :*: g) = (<>) <$> gtags f <*> gtags g

instance (GToTag f, GToTag g) => GToTag (f :+: g) where
  gtags (L1 l) = gtags l
  gtags (R1 r) = gtags r

instance GToTag (K1 R t) where
  gtags _ = pure ()

instance GToTag Par1 where
  gtags _ = pure ()

instance ToTag t => GToTag (Rec1 t) where
  gtags = tags . unRec1

instance (Foldable f, GToTag g) => GToTag (f :.: g) where
  gtags = mapM_ gtags . unComp1

instance GToTag U1 where
  gtags _ = pure ()
