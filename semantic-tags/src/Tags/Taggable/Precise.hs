{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DisambiguateRecordFields, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, OverloadedStrings, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Tags.Taggable.Precise
( runTagging
) where

import           Control.Effect.Reader
import           Control.Effect.Writer
import           Data.Foldable (traverse_)
import           Data.Maybe (listToMaybe)
import           Data.Monoid (Endo(..))
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text as T
import           GHC.Generics
import           Source.Loc
import           Source.Range
import           Source.Source
import           Tags.Tag
import qualified TreeSitter.Python.AST as Py

runTagging :: Source -> Py.Module Loc -> [Tag]
runTagging source
  = ($ [])
  . appEndo
  . run
  . execWriter
  . runReader source
  . tag where

class ToTag t where
  tag
    :: ( Carrier sig m
       , Member (Reader Source) sig
       , Member (Writer (Endo [Tag])) sig
       )
    => t Loc
    -> m ()

instance (ToTagBy strategy t, strategy ~ ToTagInstance t) => ToTag t where
  tag = tag' @strategy


class ToTagBy (strategy :: Strategy) t where
  tag'
    :: ( Carrier sig m
       , Member (Reader Source) sig
       , Member (Writer (Endo [Tag])) sig
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
  tag' (L1 l) = tag l
  tag' (R1 r) = tag r

instance ToTagBy 'Custom Py.FunctionDefinition where
  tag' Py.FunctionDefinition
    { ann = Loc Range { start } span
    , name = Py.Identifier { bytes = name }
    , parameters
    , returnType
    , body = Py.Block { ann = Loc Range { start = end } _, extraChildren }
    } = do
      src <- ask @Source
      let docs = listToMaybe extraChildren >>= docComment src
          sliced = slice src (Range start end)
      yield (Tag name Function span (firstLine sliced) docs)
      tag parameters
      traverse_ tag returnType
      traverse_ tag extraChildren

instance ToTagBy 'Custom Py.ClassDefinition where
  tag' Py.ClassDefinition
    { ann = Loc Range { start } span
    , name = Py.Identifier { bytes = name }
    , superclasses
    , body = Py.Block { ann = Loc Range { start = end } _, extraChildren }
    } = do
      src <- ask @Source
      let docs = listToMaybe extraChildren >>= docComment src
          sliced = slice src (Range start end)
      yield (Tag name Class span (firstLine sliced) docs)
      traverse_ tag superclasses
      traverse_ tag extraChildren

instance ToTagBy 'Custom Py.Call where
  tag' Py.Call
    { ann = Loc range span
    , function = Py.IdentifierPrimaryExpression Py.Identifier { bytes = name }
    , arguments
    } = do
      src <- ask @Source
      let sliced = slice src range
      yield (Tag name Call span (firstLine sliced) Nothing)
      tag arguments
  tag' Py.Call {} = pure ()

yield :: (Carrier sig m, Member (Writer (Endo [Tag])) sig) => Tag -> m ()
yield = tell . Endo . (:)

docComment :: Source -> (Py.CompoundStatement :+: Py.SimpleStatement) Loc -> Maybe Text
docComment src (R1 (Py.ExpressionStatementSimpleStatement (Py.ExpressionStatement { extraChildren = L1 (Py.PrimaryExpressionExpression (Py.StringPrimaryExpression Py.String { ann })) :|_ }))) = Just (toText (slice src (byteRange ann)))
docComment _ _ = Nothing

firstLine :: Source -> Text
firstLine = T.take 180 . T.takeWhile (/= '\n') . toText

instance (Generic1 t, GToTag (Rep1 t)) => ToTagBy 'Generic t where
  tag' = gtag . from1

class GToTag t where
  gtag
    :: ( Carrier sig m
       , Member (Reader Source) sig
       , Member (Writer (Endo [Tag])) sig
       )
    => t Loc
    -> m ()


instance GToTag f => GToTag (M1 i c f) where
  gtag = gtag . unM1

instance (GToTag f, GToTag g) => GToTag (f :*: g) where
  gtag (f :*: g) = (<>) <$> gtag f <*> gtag g

instance (GToTag f, GToTag g) => GToTag (f :+: g) where
  gtag (L1 l) = gtag l
  gtag (R1 r) = gtag r

instance GToTag (K1 R t) where
  gtag _ = pure ()

instance GToTag Par1 where
  gtag _ = pure ()

instance ToTag t => GToTag (Rec1 t) where
  gtag = tag . unRec1

instance (Foldable f, GToTag g) => GToTag (f :.: g) where
  gtag = mapM_ gtag . unComp1

instance GToTag U1 where
  gtag _ = pure mempty


class Element sub sup where
  prj :: sup a -> Maybe (sub a)

instance {-# OVERLAPPABLE #-}
         Element t t where
  prj = Just

instance {-# OVERLAPPABLE #-}
         Element t (l1 :+: l2 :+: r)
      => Element t ((l1 :+: l2) :+: r) where
  prj = prj . reassoc where
    reassoc (L1 (L1 l)) = L1 l
    reassoc (L1 (R1 l)) = R1 (L1 l)
    reassoc (R1 r)      = R1 (R1 r)

instance {-# OVERLAPPABLE #-}
         Element t (t :+: r) where
  prj (L1 l) = Just l
  prj _      = Nothing

instance {-# OVERLAPPABLE #-}
         Element t r
      => Element t (l :+: r) where
  prj (R1 r) = prj r
  prj _      = Nothing


class GSum t where
  type Members t :: (* -> *)
  gmembers :: t a -> Members t a

instance GSum f => GSum (M1 i c f) where
  type Members (M1 i c f) = Members f
  gmembers = gmembers . unM1

instance (GSum f, GSum g) => GSum (f :+: g) where
  type Members (f :+: g) = Members f :+: Members g
  gmembers (L1 l) = L1 (gmembers l)
  gmembers (R1 r) = R1 (gmembers r)
