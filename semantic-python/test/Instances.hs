{-# LANGUAGE DeriveAnyClass, DerivingStrategies, GeneralizedNewtypeDeriving, LambdaCase, StandaloneDeriving, FlexibleInstances, NamedFieldPuns, OverloadedStrings, QuantifiedConstraints, TypeOperators, UndecidableInstances, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances () where

-- Testing code depends on certain instances that we don't want to
-- expose in semantic-core proper, yet are important enough that
-- we should keep track of them in a dedicated file.

import           Analysis.ScopeGraph
import           Control.Effect.Sum
import           Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import           Data.Loc
import           Data.Core (Core, Ann (..))
import qualified Data.Map as Map
import           Data.File
import           Data.Term
import           Data.Text (Text)
import           Data.Scope (Scope, Incr)
import qualified Data.Scope as Scope
import           Data.Name

instance ToJSON a => ToJSON (Named a) where
  toJSON _ = object []

instance ToJSON1 Named where
  liftToJSON f _ (Named i a) = object
    [ "name" .= i
    , "value" .= f a
    ]

  -- Loses information compared to the toJSON instance
  -- due to an infelicity in how Aeson's toJSON1 is implemented.
  -- The correct thing to do here is to manually munge the bytestring
  -- together as a builder, but we don't even hit this code path,
  -- so it will do for now.
  liftToEncoding f _ (Named name a) = f a

instance ToJSON2 Incr where
  liftToJSON2 f _ g _ = \case
    Scope.Z a -> f a
    Scope.S b -> g b
  liftToEncoding2 f _ g _ = \case
    Scope.Z a -> f a
    Scope.S b -> g b

deriving newtype instance (ToJSON a) => ToJSON (Ignored a)

instance (Functor f, ToJSON1 f, ToJSON a) => ToJSON1 (Scope a f) where
  liftToJSON f g (Scope.Scope a) = toJSON1 (fmap (toJSON2 . fmap (liftToJSON f g)) a)
  liftToEncoding f g (Scope.Scope a) = liftToEncoding inner outer a where
    inner = liftToEncoding2 toEncoding toEncodingList hoist loist
    outer = liftToEncodingList2 toEncoding toEncodingList hoist loist
    hoist = liftToEncoding f g
    loist = liftToEncodingList f g

deriving anyclass instance (Functor f, ToJSON1 f) => ToJSON1 (Core f)

instance (ToJSON1 (sig (Term sig))) => ToJSON1 (Term sig) where
  liftToJSON f _ (Var a) = f a
  liftToJSON f g (Term s) = liftToJSON f g s

  liftToEncoding f _ (Var a) = f a
  liftToEncoding f g (Term s) = liftToEncoding f g s

instance (ToJSON1 (f k), ToJSON1 (g k)) => ToJSON1 ((:+:) f g k) where
  liftToJSON f g (L h) = liftToJSON f g h
  liftToJSON f g (R h) = liftToJSON f g h

instance (ToJSON1 f) => ToJSON1 (Ann f) where
  liftToJSON f g (Ann loc term) =
    let
      rest = case liftToJSON f g term of
        Object os -> HashMap.toList os
        other     -> ["value" .= other]
    in object (["location" .= loc] <> rest)

-- We default to deriving the default toEncoding definition (that piggybacks
-- off of toJSON) so that we never hit the problematic code paths associated
-- with toEncoding above.

instance ToJSON a => ToJSON (File a) where
  toJSON File{fileLoc, fileBody} = object
    [ "location" .= fileLoc
    , "body" .= fileBody
    ]

instance ToJSON Span where
  toJSON Span{spanStart, spanEnd} = object
    [ "kind"  .= ("span" :: Text)
    , "start" .= spanStart
    , "end"   .= spanEnd
    ]

instance ToJSON Pos where
  toJSON Pos{posLine, posCol} = object
    [ "kind" .= ("pos" :: Text)
    , "line" .= posLine
    , "column"  .= posCol
    ]

instance ToJSON Loc where
  toJSON Loc{locPath, locSpan} = object
    [ "kind" .= ("loc" :: Text)
    , "path" .= locPath
    , "span" .= locSpan
    ]

instance ToJSON Ref where
  toJSON (Ref loc) = object [ "kind" .= ("ref" :: Text)
                            , "location" .= loc]

instance ToJSON Decl where
  toJSON Decl{declSymbol, declLoc} = object
    [ "kind"   .= ("decl" :: Text)
    , "symbol" .= declSymbol
    , "location" .= declLoc
    ]

instance ToJSON ScopeGraph where
  toJSON (ScopeGraph sc) = toJSON . Map.mapKeys declSymbol $ sc
