{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, RankNTypes, TypeFamilies, TypeOperators #-}
module Diff where

import Data.Aeson
import Control.Monad (join)
import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Both (Both, Join(..), liftShowsPrecBoth)
import qualified Data.Functor.Both as Both
import Data.Functor.Classes
import Data.Functor.Classes.Pretty.Generic as Pretty
import Data.Functor.Foldable hiding (fold)
import Data.JSON.Fields
import Data.Maybe (fromMaybe)
import Data.Mergeable
import Data.Record
import Data.Text (pack)
import Data.Union
import Patch
import Syntax
import Term
import Text.Show

-- | An annotated series of patches of terms.
newtype Diff syntax ann = Diff { unDiff :: DiffF syntax ann (Diff syntax ann) }

data DiffF syntax ann recur
  = Copy [(MetaVar, recur)] (Both ann) (syntax recur)
  | Var MetaVar
  | Patch (Patch (TermF syntax ann recur))
  deriving (Foldable, Functor, Traversable)

type SyntaxDiff fields = Diff Syntax (Record fields)


newtype MetaVar = MetaVar { unMetaVar :: String }
  deriving (Eq, Ord, Show)

newtype Env a = Env { unEnv :: [(MetaVar, a)] }
  deriving (Eq, Foldable, Functor, Monoid, Ord, Show, Traversable)

envExtend :: MetaVar -> a -> Env a -> Env a
envExtend var val (Env m) = Env ((var, val) : m)

envLookup :: MetaVar -> Env a -> Maybe a
envLookup var = lookup var . unEnv


evalDiff :: Functor syntax => (DiffF syntax ann a -> Env a -> a) -> Diff syntax ann -> a
evalDiff algebra = evalDiffR (\ diff env -> algebra (snd <$> diff) (snd <$> env))

evalDiffR :: Functor syntax => (DiffF syntax ann (Diff syntax ann, a) -> Env (Diff syntax ann, a) -> a) -> Diff syntax ann -> a
evalDiffR algebra = flip go mempty
  where go = para $ \ diff env -> case diff of
          Copy bindings ann syntax ->
            let evaluated = fmap (second ($ env)) <$> bindings
                extended = foldr (uncurry envExtend) env evaluated
            in algebra (Copy evaluated ann (second ($ extended) <$> syntax)) env
          Patch patch -> algebra (Patch (fmap (second ($ env)) <$> patch)) env
          Var var -> algebra (Var var) env

evalDiffRM :: (Functor syntax, Reader (Env (Diff syntax ann, Eff fs a)) :< fs) => (DiffF syntax ann (Diff syntax ann, Eff fs a) -> Eff fs a) -> Diff syntax ann -> Eff fs a
evalDiffRM algebra = go
  where go = para $ \ diff -> case diff of
          Copy bindings ann syntax -> do
            env <- ask
            let extended = foldr (uncurry envExtend) env bindings
            local (const extended) $ algebra (Copy bindings ann syntax)
          Patch patch -> algebra (Patch patch)
          Var var -> algebra (Var var)


diffSum :: (Foldable syntax, Functor syntax) => (forall a. Patch a -> Int) -> Diff syntax ann -> Int
diffSum patchCost = evalDiff $ \ diff env -> case diff of
  Copy _ _ syntax -> sum syntax
  Var v -> fromMaybe 0 (envLookup v env)
  Patch p -> patchCost p + sum (sum <$> p)

-- | The sum of the node count of the diff’s patches.
diffCost :: (Foldable syntax, Functor syntax) => Diff syntax ann -> Int
diffCost = diffSum (const 1)

diffPatches :: (Foldable syntax, Functor syntax) => Diff syntax ann -> [Patch (TermF syntax ann (Diff syntax ann))]
diffPatches = evalDiffR $ \ diff env -> case diff of
  Copy _ _ r -> foldMap snd r
  Var v -> maybe [] snd (envLookup v env)
  Patch p -> [fmap (fmap fst) p]

-- | Merge a diff using a function to provide the Term (in Maybe, to simplify recovery of the before/after state) for every Patch.
mergeMaybe :: (Mergeable syntax, Traversable syntax) => (Patch (Term syntax ann) -> Maybe (Term syntax ann)) -> (Both ann -> ann) -> Diff syntax ann -> Maybe (Term syntax ann)
mergeMaybe transform extractAnnotation = evalDiff $ \ diff env -> case diff of
  Copy _ annotations syntax -> Term . (extractAnnotation annotations :<) <$> sequenceAlt syntax
  Var v -> join (envLookup v env)
  Patch patch -> traverse sequenceA patch >>= transform . fmap Term

-- | Recover the before state of a diff.
beforeTerm :: (Mergeable syntax, Traversable syntax) => Diff syntax ann -> Maybe (Term syntax ann)
beforeTerm = mergeMaybe before Both.fst

-- | Recover the after state of a diff.
afterTerm :: (Mergeable syntax, Traversable syntax) => Diff syntax ann -> Maybe (Term syntax ann)
afterTerm = mergeMaybe after Both.snd


-- | Strips the head annotation off a diff annotated with non-empty records.
stripDiff :: Functor f
          => Diff f (Record (h ': t))
          -> Diff f (Record t)
stripDiff = fmap rtail


-- | Constructs the replacement of one value by another in an Applicative context.
replacing :: Functor syntax => Term syntax ann -> Term syntax ann -> Diff syntax ann
replacing (Term t1) (Term t2) = Diff (Patch (Replace (deleting <$> t1) (inserting <$> t2)))

-- | Constructs the insertion of a value in an Applicative context.
inserting :: Functor syntax => Term syntax ann -> Diff syntax ann
inserting = cata (Diff . Patch . Insert)

-- | Constructs the deletion of a value in an Applicative context.
deleting :: Functor syntax => Term syntax ann -> Diff syntax ann
deleting = cata (Diff . Patch . Delete)


copy :: Both ann -> syntax (Diff syntax ann) -> Diff syntax ann
copy = (Diff .) . Copy []


instance Pretty MetaVar where
  pretty (MetaVar v) = pretty v


instance Apply1 Pretty1 fs => Pretty1 (Diff (Union fs)) where
  liftPretty p pl = go where go = liftPretty2 p pl go (Pretty.list . map go) . unDiff

instance (Apply1 Pretty1 fs, Pretty ann) => Pretty (Diff (Union fs) ann) where
  pretty = liftPretty pretty prettyList

instance Apply1 Pretty1 fs => Pretty2 (DiffF (Union fs)) where
  liftPretty2 pA plA pB plB (Copy bindings (Join ann) f) = pretty ("let" :: String) <+> align (vsep (prettyKV <$> bindings)) <> line <> pretty ("in" :: String) <+> liftPretty2 pA plA pA plA ann <+> liftPrettyUnion pB plB f
    where prettyKV (var, val) = pretty var <+> pretty '=' <+> pB val
  liftPretty2 _ _ _ _ (Var v) = pretty v
  liftPretty2 pA plA pB plB (Patch p) = liftPretty (liftPretty2 pA plA pB plB) (Pretty.list . map (liftPretty2 pA plA pB plB)) p

type instance Base (Diff syntax ann) = DiffF syntax ann

instance Functor syntax => Recursive (Diff syntax ann) where project = unDiff
instance Functor syntax => Corecursive (Diff syntax ann) where embed = Diff

instance Eq1 f => Eq1 (Diff f) where
  liftEq eqA = go where go (Diff d1) (Diff d2) = liftEq2 eqA go d1 d2

instance (Eq1 f, Eq a) => Eq (Diff f a) where
  (==) = eq1

instance Eq1 f => Eq2 (DiffF f) where
  liftEq2 eqA eqB d1 d2 = case (d1, d2) of
    (Copy v1 (Join (a1, b1)) f1, Copy v2 (Join (a2, b2)) f2) -> liftEq (liftEq eqB) v1 v2 && eqA a1 a2 && eqA b1 b2 && liftEq eqB f1 f2
    (Var v1, Var v2) -> v1 == v2
    (Patch p1, Patch p2) -> liftEq (liftEq2 eqA eqB) p1 p2
    _ -> False

instance (Eq1 f, Eq a) => Eq1 (DiffF f a) where
  liftEq = liftEq2 (==)

instance (Eq1 f, Eq a, Eq b) => Eq (DiffF f a b) where
  (==) = eq1


instance Show1 f => Show1 (Diff f) where
  liftShowsPrec sp sl = go where go d = showsUnaryWith (liftShowsPrec2 sp sl go (showListWith (go 0))) "Diff" d . unDiff

instance (Show1 f, Show a) => Show (Diff f a) where
  showsPrec = showsPrec1

instance Show1 f => Show2 (DiffF f) where
  liftShowsPrec2 spA slA spB slB d diff = case diff of
    Copy bindings ann r -> showParen (d > 10) $ showString "Copy " . liftShowList spB slB bindings . showChar ' ' . liftShowsPrecBoth spA slA 11 ann . showChar ' ' . liftShowsPrec spB slB 11 r
    Var v -> showsUnaryWith showsPrec "Var" d v
    Patch patch -> showsUnaryWith (liftShowsPrec (liftShowsPrec2 spA slA spB slB) (liftShowList2 spA slA spB slB)) "Patch" d patch

instance (Show1 f, Show a) => Show1 (DiffF f a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show1 f, Show a, Show b) => Show (DiffF f a b) where
  showsPrec = showsPrec1


instance Functor f => Functor (Diff f) where
  fmap f = go where go = Diff . bimap f go . unDiff

instance Foldable f => Foldable (Diff f) where
  foldMap f = go where go = bifoldMap f go . unDiff

instance Traversable f => Traversable (Diff f) where
  traverse f = go where go = fmap Diff . bitraverse f go . unDiff


instance Functor syntax => Bifunctor (DiffF syntax) where
  bimap f g (Copy bindings anns r) = Copy (fmap g <$> bindings) (fmap f anns) (fmap g r)
  bimap _ _ (Var v) = Var v
  bimap f g (Patch patch) = Patch (bimap f g <$> patch)

instance Foldable f => Bifoldable (DiffF f) where
  bifoldMap f g (Copy vs as r) = foldMap (g . snd) vs `mappend` foldMap f as `mappend` foldMap g r
  bifoldMap _ _ (Var _) = mempty
  bifoldMap f g (Patch p) = foldMap (bifoldMap f g) p

instance Traversable f => Bitraversable (DiffF f) where
  bitraverse f g (Copy vs as r) = Copy <$> traverse (traverse g) vs <*> traverse f as <*> traverse g r
  bitraverse _ _ (Var v) = pure (Var v)
  bitraverse f g (Patch p) = Patch <$> traverse (bitraverse f g) p


instance (ToJSONFields a, ToJSONFields1 f) => ToJSON (Diff f a) where
  toJSON = object . toJSONFields
  toEncoding = pairs . mconcat . toJSONFields

instance (ToJSONFields a, ToJSONFields1 f) => ToJSONFields (Diff f a) where
  toJSONFields = toJSONFields . unDiff

instance (ToJSON b, ToJSONFields a, ToJSONFields1 f) => ToJSONFields (DiffF f a b) where
  toJSONFields (Copy vs a f)     = foldr (\ (MetaVar k, v) -> (pack k .= v :)) [] vs <> toJSONFields a <> toJSONFields1 f
  toJSONFields (Var (MetaVar v)) = [ "metavar" .= v ]
  toJSONFields (Patch a)         = toJSONFields a
