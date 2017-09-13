{-# LANGUAGE DataKinds, RankNTypes, TypeFamilies, TypeOperators #-}
module Diff where

import Data.Aeson
import Control.Monad (join)
import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable (toList)
import Data.Functor.Binding (BindingF(..), Env(..), Metavar(..), bindings, envExtend, envLookup)
import Data.Functor.Classes
import Data.Functor.Foldable hiding (fold)
import Data.JSON.Fields
import Data.Maybe (fromMaybe)
import Data.Mergeable
import Data.Record
import Patch
import Syntax
import Term
import Text.Show

-- | An annotated series of patches of terms.
newtype Diff syntax ann = Diff { unDiff :: BindingF (DiffF syntax ann) (Diff syntax ann) }

data DiffF syntax ann recur
  = Patch (Patch (TermF syntax       ann  recur))
  | Merge        (TermF syntax (ann, ann) recur)
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
  Let _ body -> case body of
    Patch patch -> patchCost patch + sum body
    Merge merge -> sum merge
  Var v -> fromMaybe 0 (envLookup v env)

-- | The sum of the node count of the diff’s patches.
diffCost :: (Foldable syntax, Functor syntax) => Diff syntax ann -> Int
diffCost = diffSum (const 1)


diffPatch :: Diff syntax ann -> Maybe (Patch (TermF syntax ann (Diff syntax ann)))
diffPatch diff = case unDiff diff of
  Let _ (Patch patch) -> Just patch
  _ -> Nothing

diffPatches :: (Foldable syntax, Functor syntax) => Diff syntax ann -> [Patch (TermF syntax ann (Diff syntax ann))]
diffPatches = evalDiffR $ \ diff env -> case diff of
  Let _ (Patch patch) -> fmap (fmap fst) patch : foldMap (foldMap (toList . diffPatch . fst)) patch
  Let _ (Merge merge) ->                                  foldMap (toList . diffPatch . fst)  merge
  Var var -> maybe [] snd (envLookup var env)


-- | Merge a diff using a function to provide the Term (in Maybe, to simplify recovery of the before/after state) for every Patch.
mergeMaybe :: (Mergeable syntax, Traversable syntax) => (DiffF syntax ann (Maybe (Term syntax ann)) -> Maybe (Term syntax ann)) -> Diff syntax ann -> Maybe (Term syntax ann)
mergeMaybe algebra = evalDiff $ \ bind env -> case bind of
  Let _ diff -> algebra diff
  Var v -> join (envLookup v env)

-- | Recover the before state of a diff.
beforeTerm :: (Mergeable syntax, Traversable syntax) => Diff syntax ann -> Maybe (Term syntax ann)
beforeTerm = mergeMaybe $ \ diff -> case diff of
  Patch patch -> before patch >>= \ (In a l) -> termIn a <$> sequenceAlt l
  Merge  (In (a, _) l) -> termIn a <$> sequenceAlt l

-- | Recover the after state of a diff.
afterTerm :: (Mergeable syntax, Traversable syntax) => Diff syntax ann -> Maybe (Term syntax ann)
afterTerm = mergeMaybe $ \ diff -> case diff of
  Patch patch -> after patch >>= \ (In b r) -> termIn b <$> sequenceAlt r
  Merge  (In (_, b) r) -> termIn b <$> sequenceAlt r


-- | Strips the head annotation off a diff annotated with non-empty records.
stripDiff :: Functor f
          => Diff f (Record (h ': t))
          -> Diff f (Record t)
stripDiff = fmap rtail


-- | Constructs the replacement of one value by another in an Applicative context.
replacing :: Functor syntax => Term syntax ann -> Term syntax ann -> Diff syntax ann
replacing (Term (In a1 r1)) (Term (In a2 r2)) = Diff (Let mempty (Patch (Replace (In a1 (deleting <$> r1)) (In a2 (inserting <$> r2)))))

-- | Constructs the insertion of a value in an Applicative context.
inserting :: Functor syntax => Term syntax ann -> Diff syntax ann
inserting = cata (Diff . Let mempty . Patch . Insert)

-- | Constructs the deletion of a value in an Applicative context.
deleting :: Functor syntax => Term syntax ann -> Diff syntax ann
deleting = cata (Diff . Let mempty . Patch . Delete)


merge :: (ann, ann) -> syntax (Diff syntax ann) -> Diff syntax ann
merge = (Diff .) . (Let mempty .) . (Merge .) . In

var :: Metavar -> Diff syntax ann
var = Diff . Var



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
    (Patch p1, Patch p2) -> liftEq (liftEq2 eqA eqB) p1 p2
    (Merge t1, Merge t2) -> liftEq2 (liftEq2 eqA eqA) eqB t1 t2
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
    Patch patch -> showsUnaryWith (liftShowsPrec (liftShowsPrec2 spA slA spB slB) (liftShowList2 spA slA spB slB)) "Patch" d patch
    Merge term  -> showsUnaryWith (liftShowsPrec2 spBoth slBoth spB slB) "Merge" d term
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
  bimap f g (Patch patch) = Patch (bimap f g <$> patch)
  bimap f g (Merge term)  = Merge (bimap (bimap f f) g term)

instance Foldable f => Bifoldable (DiffF f) where
  bifoldMap f g (Patch patch) = foldMap (bifoldMap f g) patch
  bifoldMap f g (Merge term)  = bifoldMap (bifoldMap f f) g term

instance Traversable f => Bitraversable (DiffF f) where
  bitraverse f g (Patch patch) = Patch <$> traverse (bitraverse f g) patch
  bitraverse f g (Merge term)  = Merge <$> bitraverse (bitraverse f f) g term


instance (ToJSONFields a, ToJSONFields1 f) => ToJSON (Diff f a) where
  toJSON = object . toJSONFields
  toEncoding = pairs . mconcat . toJSONFields

instance (ToJSONFields a, ToJSONFields1 f) => ToJSONFields (Diff f a) where
  toJSONFields = toJSONFields . unDiff

instance (ToJSONFields a, ToJSONFields1 f) => ToJSONFields1 (DiffF f a) where
  toJSONFields1 (Patch patch) = [ "patch" .= JSONFields patch ]
  toJSONFields1 (Merge term)  = [ "merge" .= JSONFields term ]

instance (ToJSONFields1 f, ToJSONFields a, ToJSON b) => ToJSONFields (DiffF f a b) where
  toJSONFields = toJSONFields1

instance (ToJSON b, ToJSONFields a, ToJSONFields1 f) => ToJSON (DiffF f a b) where
  toJSON = object . toJSONFields
  toEncoding = pairs . mconcat . toJSONFields
