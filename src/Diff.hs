{-# LANGUAGE DataKinds, RankNTypes, TypeFamilies, TypeOperators #-}
module Diff where

import Data.Aeson
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable (toList)
import Data.Functor.Classes
import Data.Functor.Foldable hiding (fold)
import Data.JSON.Fields
import Data.Mergeable
import Data.Record
import Patch
import Term
import Text.Show

-- | A recursive structure indicating the changed & unchanged portions of a labelled tree.
newtype Diff syntax ann = Diff { unDiff :: DiffF syntax ann (Diff syntax ann) }

-- | A single entry within a recursive 'Diff'.
data DiffF syntax ann recur
  -- | A changed node, represented as 'Insert'ed, 'Delete'd, or 'Replace'd 'TermF's, consisting of syntax labelled with an annotation.
  = Patch (Patch (TermF syntax       ann  recur)
                 (TermF syntax       ann  recur))
  -- | An unchanged node, consisting of syntax labelled with both the original annotations.
  | Merge        (TermF syntax (ann, ann) recur)

-- | Constructs a 'Diff' replacing one 'Term' with another recursively.
replacing :: Functor syntax => Term syntax ann -> Term syntax ann -> Diff syntax ann
replacing (Term (In a1 r1)) (Term (In a2 r2)) = Diff (Patch (Replace (In a1 (deleting <$> r1)) (In a2 (inserting <$> r2))))

-- | Constructs a 'Diff' inserting a 'Term' recursively.
inserting :: Functor syntax => Term syntax ann -> Diff syntax ann
inserting = cata insertF

-- | Constructs a 'Diff' inserting a single 'TermF' populated by further 'Diff's.
insertF :: TermF syntax ann (Diff syntax ann) -> Diff syntax ann
insertF = Diff . Patch . Insert

-- | Constructs a 'Diff' deleting a 'Term' recursively.
deleting :: Functor syntax => Term syntax ann -> Diff syntax ann
deleting = cata deleteF

-- | Constructs a 'Diff' deleting a single 'TermF' populated by further 'Diff's.
deleteF :: TermF syntax ann (Diff syntax ann) -> Diff syntax ann
deleteF = Diff . Patch . Delete

-- | Constructs a 'Diff' merging two annotations for a single syntax functor populated by further 'Diff's.
merge :: (ann, ann) -> syntax (Diff syntax ann) -> Diff syntax ann
merge = (Diff .) . (Merge .) . In


diffSum :: (Foldable syntax, Functor syntax) => (forall a b. Patch a b -> Int) -> Diff syntax ann -> Int
diffSum patchCost = cata $ \ diff -> case diff of
  Patch patch -> patchCost patch + sum (sum <$> patch)
  Merge merge -> sum merge

-- | The sum of the node count of the diff’s patches.
diffCost :: (Foldable syntax, Functor syntax) => Diff syntax ann -> Int
diffCost = diffSum (const 1)


diffPatch :: Diff syntax ann -> Maybe (Patch (TermF syntax ann (Diff syntax ann)) (TermF syntax ann (Diff syntax ann)))
diffPatch diff = case unDiff diff of
  Patch patch -> Just patch
  _ -> Nothing

diffPatches :: (Foldable syntax, Functor syntax) => Diff syntax ann -> [Patch (TermF syntax ann (Diff syntax ann)) (TermF syntax ann (Diff syntax ann))]
diffPatches = para $ \ diff -> case diff of
  Patch patch -> bimap (fmap fst) (fmap fst) patch : foldMap (foldMap (toList . diffPatch . fst)) patch
  Merge merge ->                                              foldMap (toList . diffPatch . fst)  merge


-- | Merge a diff using a function to provide the Term (in Maybe, to simplify recovery of the before/after state) for every Patch.
mergeMaybe :: (Mergeable syntax, Traversable syntax) => (DiffF syntax ann (Maybe (Term syntax ann)) -> Maybe (Term syntax ann)) -> Diff syntax ann -> Maybe (Term syntax ann)
mergeMaybe = cata

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


type instance Base (Diff syntax ann) = DiffF syntax ann

instance Functor syntax => Recursive   (Diff syntax ann) where project = unDiff
instance Functor syntax => Corecursive (Diff syntax ann) where embed = Diff


instance Eq1 f => Eq1 (Diff f) where
  liftEq eqA = go where go (Diff d1) (Diff d2) = liftEq2 eqA go d1 d2

instance (Eq1 f, Eq a) => Eq (Diff f a) where
  (==) = eq1

instance Eq1 f => Eq2 (DiffF f) where
  liftEq2 eqA eqB d1 d2 = case (d1, d2) of
    (Patch p1, Patch p2) -> liftEq2 (liftEq2 eqA eqB) (liftEq2 eqA eqB) p1 p2
    (Merge t1, Merge t2) -> liftEq2 (liftEq2 eqA eqA) eqB t1 t2
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
    Patch patch -> showsUnaryWith (liftShowsPrec2 (liftShowsPrec2 spA slA spB slB) (liftShowList2 spA slA spB slB) (liftShowsPrec2 spA slA spB slB) (liftShowList2 spA slA spB slB)) "Patch" d patch
    Merge term  -> showsUnaryWith (liftShowsPrec2 spBoth slBoth spB slB) "Merge" d term
    where spBoth = liftShowsPrec2 spA slA spA slA
          slBoth = liftShowList2 spA slA spA slA

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


instance Functor syntax => Functor (DiffF syntax ann) where
  fmap = second

instance Functor syntax => Bifunctor (DiffF syntax) where
  bimap f g (Patch patch) = Patch (bimap (bimap f g) (bimap f g) patch)
  bimap f g (Merge term)  = Merge (bimap (bimap f f) g term)

instance Foldable syntax => Foldable (DiffF syntax ann) where
  foldMap = bifoldMap (const mempty)

instance Foldable syntax => Bifoldable (DiffF syntax) where
  bifoldMap f g (Patch patch) = bifoldMap (bifoldMap f g) (bifoldMap f g) patch
  bifoldMap f g (Merge term)  = bifoldMap (bifoldMap f f) g term

instance Traversable syntax => Traversable (DiffF syntax ann) where
  traverse = bitraverse pure

instance Traversable syntax => Bitraversable (DiffF syntax) where
  bitraverse f g (Patch patch) = Patch <$> bitraverse (bitraverse f g) (bitraverse f g) patch
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
