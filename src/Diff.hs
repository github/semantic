{-# LANGUAGE DataKinds, RankNTypes, TypeFamilies, TypeOperators #-}
module Diff where

import Data.Aeson
import Control.Monad (join)
import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Binding (BindingF(..), Env(..), Metavar(..), bindings, envExtend, envLookup)
import Data.Functor.Classes
import Data.Functor.Classes.Pretty.Generic as Pretty
import Data.Functor.Foldable hiding (fold)
import Data.Functor.Product as Product
import Data.Functor.Sum as Sum
import Data.JSON.Fields
import Data.Maybe (fromMaybe)
import Data.Mergeable
import Data.Record
import Data.Union
import Patch
import Syntax
import Term
import Text.Show

-- | An annotated series of patches of terms.
newtype Diff syntax ann = Diff { unDiff :: BindingF (DiffF syntax ann) (Diff syntax ann) }

data DiffF syntax ann recur
  = Either (TermF (Sum     syntax syntax)       ann  recur)
  | Both   (TermF (Product syntax syntax) (ann, ann) recur)
  | Merge  (TermF                 syntax  (ann, ann) recur)
  deriving (Foldable, Functor, Traversable)

type SyntaxDiff fields = Diff Syntax (Record fields)


evalDiff :: Functor syntax => (BindingF (DiffF syntax ann) a -> Env a -> a) -> Diff syntax ann -> a
evalDiff algebra = evalDiffR (\ diff env -> algebra (snd <$> diff) (snd <$> env))

evalDiffR :: Functor syntax => (BindingF (DiffF syntax ann) (Diff syntax ann, a) -> Env (Diff syntax ann, a) -> a) -> Diff syntax ann -> a
evalDiffR algebra = flip (para evalBinding) mempty
  where evalBinding bind env = case bind of
          Let vars body ->
            let evaluated = second ($ env) <$> vars
                extended = foldr (uncurry envExtend) env (unEnv evaluated)
            in algebra (Let evaluated (second ($ extended) <$> body)) env
          _ -> algebra (second ($ env) <$> bind) env

evalDiffRM :: (Functor syntax, Reader (Env (Diff syntax ann, Eff fs a)) :< fs) => (BindingF (DiffF syntax ann) (Diff syntax ann, Eff fs a) -> Eff fs a) -> Diff syntax ann -> Eff fs a
evalDiffRM algebra = para (\ diff -> local (bindMetavariables diff) (algebra diff))
  where bindMetavariables diff env = foldr (uncurry envExtend) env (unEnv (bindings diff))


diffSum :: (Foldable syntax, Functor syntax) => (forall a. Patch a -> Int) -> Diff syntax ann -> Int
diffSum patchCost = evalDiff $ \ diff env -> case diff of
  Let _ (Either (In _ (InL l))) -> patchCost (Delete ()) + sum l
  Let _ (Either (In _ (InR r))) -> patchCost (Insert ()) + sum r
  Let _ (Both   (In _ (Product.Pair l r))) -> patchCost (Replace () ()) + sum l + sum r
  Let _ (Merge  (In _ lr)) -> sum lr
  Var v -> fromMaybe 0 (envLookup v env)

-- | The sum of the node count of the diff’s patches.
diffCost :: (Foldable syntax, Functor syntax) => Diff syntax ann -> Int
diffCost = diffSum (const 1)


-- | Merge a diff using a function to provide the Term (in Maybe, to simplify recovery of the before/after state) for every Patch.
mergeMaybe :: (Mergeable syntax, Traversable syntax) => (DiffF syntax ann (Maybe (Term syntax ann)) -> Maybe (Term syntax ann)) -> Diff syntax ann -> Maybe (Term syntax ann)
mergeMaybe algebra = evalDiff $ \ bind env -> case bind of
  Let _ diff -> algebra diff
  Var v -> join (envLookup v env)

-- | Recover the before state of a diff.
beforeTerm :: (Mergeable syntax, Traversable syntax) => Diff syntax ann -> Maybe (Term syntax ann)
beforeTerm = mergeMaybe $ \ diff -> case diff of
  Either (In a (InL l)) -> termIn a <$> sequenceAlt l
  Either (In _ (InR _)) -> Nothing
  Both   (In (a, _) (Product.Pair l _)) -> termIn a <$> sequenceAlt l
  Merge  (In (a, _) l) -> termIn a <$> sequenceAlt l

-- | Recover the after state of a diff.
afterTerm :: (Mergeable syntax, Traversable syntax) => Diff syntax ann -> Maybe (Term syntax ann)
afterTerm = mergeMaybe $ \ diff -> case diff of
  Either (In _ (InL _)) -> Nothing
  Either (In b (InR r)) -> termIn b <$> sequenceAlt r
  Both   (In (_, b) (Product.Pair _ r)) -> termIn b <$> sequenceAlt r
  Merge  (In (_, b) r) -> termIn b <$> sequenceAlt r


-- | Strips the head annotation off a diff annotated with non-empty records.
stripDiff :: Functor f
          => Diff f (Record (h ': t))
          -> Diff f (Record t)
stripDiff = fmap rtail


-- | Constructs the replacement of one value by another in an Applicative context.
replacing :: Functor syntax => Term syntax ann -> Term syntax ann -> Diff syntax ann
replacing (Term (In a1 r1)) (Term (In a2 r2)) = Diff (Let mempty (Both (In (a1, a2) (Product.Pair (deleting <$> r1) (inserting <$> r2)))))

-- | Constructs the insertion of a value in an Applicative context.
inserting :: Functor syntax => Term syntax ann -> Diff syntax ann
inserting = cata (Diff . Let mempty . Either . hoistTermF InR)

-- | Constructs the deletion of a value in an Applicative context.
deleting :: Functor syntax => Term syntax ann -> Diff syntax ann
deleting = cata (Diff . Let mempty . Either . hoistTermF InL)


copy :: (ann, ann) -> syntax (Diff syntax ann) -> Diff syntax ann
copy = (Diff .) . (Let mempty .) . (Merge .) . In

var :: Metavar -> Diff syntax ann
var = Diff . Var



instance Apply1 Pretty1 fs => Pretty1 (Diff (Union fs)) where
  liftPretty p pl = go
    where go = pretty' . unDiff
          pretty' (Let vars body) = pretty ("let" :: String) <+> align (vsep (prettyKV <$> unEnv vars)) <> line
                                 <> pretty ("in" :: String)  <+> liftPretty2 p pl go (Pretty.list . map go) body
          pretty' (Var var) = pretty var
          prettyKV (var, val) = pretty var <+> pretty '=' <+> go val

instance (Apply1 Pretty1 fs, Pretty ann) => Pretty (Diff (Union fs) ann) where
  pretty = liftPretty pretty prettyList

instance Apply1 Pretty1 fs => Pretty2 (DiffF (Union fs)) where
  liftPretty2 pA plA pB plB diff = case diff of
    Either term -> pretty ("either" :: String) <+> liftPretty2 pA plA pB plB term
    Both   term -> pretty ("both"   :: String) <+> liftPretty2 pBoth plBoth pB plB term
    Merge  term -> pretty ("merge"  :: String) <+> liftPretty2 pBoth plBoth pB plB term
    where pBoth = liftPretty2 pA plA pA plA
          plBoth = Pretty.list . map pBoth

instance (Apply1 Pretty1 fs, Pretty ann) => Pretty1 (DiffF (Union fs) ann) where
  liftPretty = liftPretty2 pretty prettyList


type instance Base (Diff syntax ann) = BindingF (DiffF syntax ann)

instance Functor syntax => Recursive   (Diff syntax ann) where project = unDiff
instance Functor syntax => Corecursive (Diff syntax ann) where embed = Diff


instance Eq1 f => Eq1 (Diff f) where
  liftEq eqA = go
    where go (Diff d1) (Diff d2) = eq' d1 d2
          eq' (Let v1 b1) (Let v2 b2) = liftEq go v1 v2 && liftEq2 eqA go b1 b2
          eq' (Var v1)    (Var v2)    = v1 == v2
          eq' _           _           = False

instance (Eq1 f, Eq a) => Eq (Diff f a) where
  (==) = eq1

instance Eq1 f => Eq2 (DiffF f) where
  liftEq2 eqA eqB d1 d2 = case (d1, d2) of
    (Either t1, Either t2) -> liftEq2 eqA eqB t1 t2
    (Both   t1, Both   t2) -> liftEq2 (liftEq2 eqA eqA) eqB t1 t2
    (Merge  t1, Merge  t2) -> liftEq2 (liftEq2 eqA eqA) eqB t1 t2
    _ -> False

instance (Eq1 f, Eq a) => Eq1 (DiffF f a) where
  liftEq = liftEq2 (==)

instance (Eq1 f, Eq a, Eq b) => Eq (DiffF f a b) where
  (==) = eq1


instance Show1 f => Show1 (Diff f) where
  liftShowsPrec sp sl = go
    where go d = showsUnaryWith showsPrec' "Diff" d . unDiff
          showsPrec' d (Let vars body) = showsBinaryWith (liftShowsPrec go (showListWith (go 0))) (liftShowsPrec2 sp sl go (showListWith (go 0))) "Let" d vars body
          showsPrec' d (Var var)       = showsUnaryWith showsPrec "Var" d var

instance (Show1 f, Show a) => Show (Diff f a) where
  showsPrec = showsPrec1

instance Show1 f => Show2 (DiffF f) where
  liftShowsPrec2 spA slA spB slB d diff = case diff of
    Either term -> showsUnaryWith (liftShowsPrec2 spA slA spB slB) "Either" d term
    Both   term -> showsUnaryWith (liftShowsPrec2 spBoth slBoth spB slB) "Both"   d term
    Merge  term -> showsUnaryWith (liftShowsPrec2 spBoth slBoth spB slB) "Merge"  d term
    where spBoth = liftShowsPrec2 spA slA spA slA
          slBoth = liftShowList2 spA slA spA slA

instance (Show1 f, Show a) => Show1 (DiffF f a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show1 f, Show a, Show b) => Show (DiffF f a b) where
  showsPrec = showsPrec1


instance Functor f => Functor (Diff f) where
  fmap f = go
    where go = Diff . fmap' . unDiff
          fmap' (Let vars body) = Let (fmap go vars) (bimap f go body)
          fmap' (Var var)       = Var var

instance Foldable f => Foldable (Diff f) where
  foldMap f = go
    where go = foldMap' . unDiff
          foldMap' (Let vars body) = foldMap go vars `mappend` bifoldMap f go body
          foldMap' _               = mempty

instance Traversable f => Traversable (Diff f) where
  traverse f = go
    where go = fmap Diff . traverse' . unDiff
          traverse' (Let vars body) = Let <$> traverse go vars <*> bitraverse f go body
          traverse' (Var v)         = pure (Var v)


instance Functor syntax => Bifunctor (DiffF syntax) where
  bimap f g (Either term) = Either (bimap f g term)
  bimap f g (Both   term) = Both   (bimap (bimap f f) g term)
  bimap f g (Merge  term) = Merge  (bimap (bimap f f) g term)

instance Foldable f => Bifoldable (DiffF f) where
  bifoldMap f g (Either term) = bifoldMap f g term
  bifoldMap f g (Both   term) = bifoldMap (bifoldMap f f) g term
  bifoldMap f g (Merge  term) = bifoldMap (bifoldMap f f) g term

instance Traversable f => Bitraversable (DiffF f) where
  bitraverse f g (Either term) = Either <$> bitraverse f g term
  bitraverse f g (Both   term) = Both   <$> bitraverse (bitraverse f f) g term
  bitraverse f g (Merge  term) = Merge  <$> bitraverse (bitraverse f f) g term


instance (ToJSONFields a, ToJSONFields1 f) => ToJSON (Diff f a) where
  toJSON = object . toJSONFields
  toEncoding = pairs . mconcat . toJSONFields

instance (ToJSONFields a, ToJSONFields1 f) => ToJSONFields (Diff f a) where
  toJSONFields = toJSONFields . unDiff

instance (ToJSONFields a, ToJSONFields1 f) => ToJSONFields1 (DiffF f a) where
  toJSONFields1 (Either term) = [ "either" .= JSONFields term ]
  toJSONFields1 (Both   term) = [ "both"   .= JSONFields term ]
  toJSONFields1 (Merge  term) = [ "merge"  .= JSONFields term ]

instance (ToJSONFields1 f, ToJSONFields a, ToJSON b) => ToJSONFields (DiffF f a b) where
  toJSONFields = toJSONFields1

instance (ToJSON b, ToJSONFields a, ToJSONFields1 f) => ToJSON (DiffF f a b) where
  toJSON = object . toJSONFields
  toEncoding = pairs . mconcat . toJSONFields
