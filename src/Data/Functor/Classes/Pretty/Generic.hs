{-# LANGUAGE TypeOperators #-}
module Data.Functor.Classes.Pretty.Generic
( module Pretty
, genericLiftPretty
) where

import Data.Text.Prettyprint.Doc as Pretty
import GHC.Generics

genericLiftPretty :: (Generic1 f, GPretty1 (Rep1 f)) => (a -> Doc ann) -> ([a] -> Doc ann) -> f a -> Doc ann
genericLiftPretty pretty' prettyList' = gliftPretty pretty' prettyList' . from1


class GPretty1 f where
  gliftPretty :: (a -> Doc ann) -> ([a] -> Doc ann) -> f a -> Doc ann
  gcollectPretty :: (a -> Doc ann) -> ([a] -> Doc ann) -> f a -> [Doc ann]
  gcollectPretty p pl a = [gliftPretty p pl a]

instance GPretty1 U1 where
  gliftPretty _ _ _ = emptyDoc

instance GPretty1 Par1 where
  gliftPretty p _ (Par1 a) = p a

instance Pretty c => GPretty1 (K1 i c) where
  gliftPretty _ _ (K1 a) = pretty a

instance Pretty1 f => GPretty1 (Rec1 f) where
  gliftPretty p pl (Rec1 a) = liftPretty p pl a

instance GPretty1 f => GPretty1 (M1 D c f) where
  gliftPretty p pl (M1 a) = gliftPretty p pl a

instance (Constructor c, GPretty1 f) => GPretty1 (M1 C c f) where
  gliftPretty p pl m = nest 2 (vsep (pretty (conName m) : gcollectPretty p pl (unM1 m)))

instance GPretty1 f => GPretty1 (M1 S c f) where
  gliftPretty p pl (M1 a) = gliftPretty p pl a

instance (GPretty1 f, GPretty1 g) => GPretty1 (f :+: g) where
  gliftPretty p pl (L1 l) = gliftPretty p pl l
  gliftPretty p pl (R1 r) = gliftPretty p pl r

instance (GPretty1 f, GPretty1 g) => GPretty1 (f :*: g) where
  gliftPretty p pl (a :*: b) = gliftPretty p pl a <+> gliftPretty p pl b
  gcollectPretty p pl (a :*: b) = gcollectPretty p pl a <> gcollectPretty p pl b

instance (Pretty1 f, GPretty1 g) => GPretty1 (f :.: g) where
  gliftPretty p pl (Comp1 a) = liftPretty (gliftPretty p pl) (list . map (gliftPretty p pl)) a
