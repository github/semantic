{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DisambiguateRecordFields, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, OverloadedStrings, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
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
    => t
    -> m ()

instance (ToTagBy strategy t, strategy ~ ToTagInstance t) => ToTag t where
  tag = tag' @strategy


class ToTagBy (strategy :: Strategy) t where
  tag'
    :: ( Carrier sig m
       , Member (Reader Source) sig
       , Member (Writer (Endo [Tag])) sig
       )
    => t
    -> m ()


data Strategy = Generic | Custom

type family ToTagInstance t :: Strategy where
  ToTagInstance Loc                         = 'Custom
  ToTagInstance Text                        = 'Custom
  ToTagInstance [_]                         = 'Custom
  ToTagInstance (Either _ _)                = 'Custom
  ToTagInstance (Py.FunctionDefinition Loc) = 'Custom
  ToTagInstance _                           = 'Generic

instance ToTagBy 'Custom Loc where
  tag' _ = pure ()

instance ToTagBy 'Custom Text where
  tag' _ = pure ()

instance ToTag t => ToTagBy 'Custom [t] where
  tag' = traverse_ tag

instance (ToTag l, ToTag r) => ToTagBy 'Custom (Either l r) where
  tag' = either tag tag

instance ToTagBy 'Custom (Py.FunctionDefinition Loc) where
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
      yield (Tag name Function span (Just (firstLine sliced)) docs)
      tag parameters
      tag returnType
      traverse_ tag extraChildren

yield :: (Carrier sig m, Member (Writer (Endo [Tag])) sig) => Tag -> m ()
yield = tell . Endo . (:)

docComment :: Source -> Either (Py.CompoundStatement Loc) (Py.SimpleStatement Loc) -> Maybe Text
docComment src (Right (Py.ExpressionStatementSimpleStatement (Py.ExpressionStatement { extraChildren = Left (Py.PrimaryExpressionExpression (Py.StringPrimaryExpression Py.String { ann })) :|_ }))) = Just (toText (slice src (byteRange ann)))
docComment _ _ = Nothing

firstLine :: Source -> Text
firstLine = T.take 180 . T.takeWhile (/= '\n') . toText

instance (Generic t, GToTag (Rep t)) => ToTagBy 'Generic t where
  tag' = gtag . from


class GToTag t where
  gtag
    :: ( Carrier sig m
       , Member (Reader Source) sig
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
