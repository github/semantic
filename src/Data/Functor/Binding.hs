{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
module Data.Functor.Binding where

import Data.Aeson (KeyValue(..), ToJSON)
import Data.Functor.Classes
import Data.JSON.Fields
import Data.Text.Prettyprint.Doc

newtype Metavar = Metavar Int
  deriving (Eq, Ord, Show)
  deriving newtype (Enum, ToJSON)


data BindingF f recur
  = Let [(Metavar, recur)] (f recur)
  | VarF Metavar

bindings :: BindingF f recur -> [(Metavar, recur)]
bindings (Let vars _) = vars
bindings _            = []


newtype Env a = Env { unEnv :: [(Metavar, a)] }
  deriving (Eq, Foldable, Functor, Monoid, Ord, Show, Traversable)

envExtend :: Metavar -> a -> Env a -> Env a
envExtend var val (Env m) = Env ((var, val) : m)

envLookup :: Metavar -> Env a -> Maybe a
envLookup var = lookup var . unEnv


instance Eq1 f => Eq1 (BindingF f) where
  liftEq eq (Let v1 b1) (Let v2 b2) = liftEq (liftEq eq) v1 v2 && liftEq eq b1 b2
  liftEq _  (VarF v1)   (VarF v2)   = v1 == v2
  liftEq _  _           _           = False

instance (Eq1 f, Eq a) => Eq (BindingF f a) where
  (==) = eq1


instance Show1 f => Show1 (BindingF f) where
  liftShowsPrec sp sl d (Let vars body) = showsBinaryWith (const (liftShowList sp sl)) (liftShowsPrec sp sl) "Let" d vars body
  liftShowsPrec _  _  d (VarF var)      = showsUnaryWith showsPrec "Var" d var

instance (Show1 f, Show a) => Show (BindingF f a) where
  showsPrec = showsPrec1


instance Pretty Metavar where
  pretty (Metavar v) = pretty v

instance Pretty1 f => Pretty1 (BindingF f) where
  liftPretty p pl (Let vars body) = pretty ("let" :: String) <+> align (vsep (prettyKV <$> vars)) <> line
                                 <> pretty ("in" :: String)  <+> liftPretty p pl body
    where prettyKV (var, val) = pretty var <+> pretty '=' <+> p val
  liftPretty _ _  (VarF metavar)  = pretty metavar

instance (Pretty1 f, Pretty a) => Pretty (BindingF f a) where
  pretty = liftPretty pretty prettyList


instance ToJSONFields1 f => ToJSONFields1 (BindingF f) where
  toJSONFields1 (Let vars body) = [ "vars" .= vars ] <> toJSONFields1 body
  toJSONFields1 (VarF v)        = [ "metavar" .= v ]
