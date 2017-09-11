{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
module Data.Functor.Binding where

import Data.Aeson (ToJSON)
import Data.Text.Prettyprint.Doc

newtype Metavar = Metavar Int
  deriving (Eq, Ord, Show)
  deriving newtype (Enum, ToJSON)


data BindingF f recur
  = Let [(Metavar, recur)] (f recur)
  | VarF Metavar


newtype Env a = Env { unEnv :: [(Metavar, a)] }
  deriving (Eq, Foldable, Functor, Monoid, Ord, Show, Traversable)

envExtend :: Metavar -> a -> Env a -> Env a
envExtend var val (Env m) = Env ((var, val) : m)

envLookup :: Metavar -> Env a -> Maybe a
envLookup var = lookup var . unEnv


instance Pretty Metavar where
  pretty (Metavar v) = pretty v

instance Pretty1 f => Pretty1 (BindingF f) where
  liftPretty p pl (Let vars body) = pretty ("let" :: String) <+> align (vsep (prettyKV <$> vars)) <> line
                                 <> pretty ("in" :: String)  <+> liftPretty p pl body
    where prettyKV (var, val) = pretty var <+> pretty '=' <+> p val
  liftPretty _ _  (VarF metavar)  = pretty metavar
