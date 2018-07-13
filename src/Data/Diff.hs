{-# LANGUAGE DataKinds, RankNTypes, TypeFamilies, TypeOperators, ScopedTypeVariables, UndecidableInstances #-}
module Data.Diff
( Diff(..)
, DiffF(..)
, replacing
, replaceF
, inserting
, insertF
, deleting
, deleteF
, merge
, mergeF
, merging
, diffPatches
, beforeTerm
, afterTerm
, stripDiff
) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable (asum)
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.JSON.Fields
import Data.Mergeable (Mergeable(sequenceAlt))
import Data.Patch
import Data.Record
import Data.Term
import Text.Show
import Prologue
import Proto3.Suite.Class
import Proto3.Suite.DotProto
import qualified Proto3.Wire.Encode as Encode
import qualified Proto3.Wire.Decode as Decode

-- | A recursive structure indicating the changed & unchanged portions of a labelled tree.
newtype Diff syntax ann1 ann2 = Diff { unDiff :: DiffF syntax ann1 ann2 (Diff syntax ann1 ann2) }

-- | A single entry within a recursive 'Diff'.
data DiffF syntax ann1 ann2 recur
  -- | A changed node, represented as 'Insert'ed, 'Delete'd, or 'Replace'd 'TermF's, consisting of syntax labelled with an annotation.
  = Patch (Patch (TermF syntax  ann1        recur)
                 (TermF syntax        ann2  recur))
  -- | An unchanged node, consisting of syntax labelled with both the original annotations.
  | Merge        (TermF syntax (ann1, ann2) recur)

-- | Constructs a 'Diff' replacing one 'Term' with another recursively.
replacing :: Functor syntax => Term syntax ann1 -> Term syntax ann2 -> Diff syntax ann1 ann2
replacing (Term (In a1 r1)) (Term (In a2 r2)) = replaceF (In a1 (deleting <$> r1)) (In a2 (inserting <$> r2))

-- | Constructs a 'Diff' replacing one 'TermF' populated by further 'Diff's with another.
replaceF :: TermF syntax ann1 (Diff syntax ann1 ann2) -> TermF syntax ann2 (Diff syntax ann1 ann2) -> Diff syntax ann1 ann2
replaceF t1 t2 = Diff (Patch (Replace t1 t2))

-- | Constructs a 'Diff' inserting a 'Term' recursively.
inserting :: Functor syntax => Term syntax ann2 -> Diff syntax ann1 ann2
inserting = cata insertF

-- | Constructs a 'Diff' inserting a single 'TermF' populated by further 'Diff's.
insertF :: TermF syntax ann2 (Diff syntax ann1 ann2) -> Diff syntax ann1 ann2
insertF = Diff . Patch . Insert

-- | Constructs a 'Diff' deleting a 'Term' recursively.
deleting :: Functor syntax => Term syntax ann1 -> Diff syntax ann1 ann2
deleting = cata deleteF

-- | Constructs a 'Diff' deleting a single 'TermF' populated by further 'Diff's.
deleteF :: TermF syntax ann1 (Diff syntax ann1 ann2) -> Diff syntax ann1 ann2
deleteF = Diff . Patch . Delete

-- | Constructs a 'Diff' merging two annotations for a single syntax functor populated by further 'Diff's.
merge :: (ann1, ann2) -> syntax (Diff syntax ann1 ann2) -> Diff syntax ann1 ann2
merge = fmap mergeF . In

-- | Constructs a 'Diff' merging a single 'TermF' populated by further 'Diff's.
mergeF :: TermF syntax (ann1, ann2) (Diff syntax ann1 ann2) -> Diff syntax ann1 ann2
mergeF = Diff . Merge

-- | Constructs a 'Diff' merging a 'Term' recursively.
--
--   Note that since this simply duplicates the 'Term'’s annotations, it is only really useful in tests or other contexts where preserving annotations from both sides is unnecessary.
merging :: Functor syntax => Term syntax ann -> Diff syntax ann ann
merging = cata (\ (In ann syntax) -> mergeF (In (ann, ann) syntax))


diffPatches :: (Foldable syntax, Functor syntax) => Diff syntax ann1 ann2 -> [Patch (TermF syntax ann1 (Diff syntax ann1 ann2)) (TermF syntax ann2 (Diff syntax ann1 ann2))]
diffPatches = para $ \ diff -> case diff of
  Patch patch -> bimap (fmap fst) (fmap fst) patch : bifoldMap (foldMap snd) (foldMap snd) patch
  Merge merge -> foldMap snd merge


-- | Recover the before state of a diff.
beforeTerm :: (Foldable syntax, Mergeable syntax) => Diff syntax ann1 ann2 -> Maybe (Term syntax ann1)
beforeTerm = cata $ \ diff -> case diff of
  Patch patch -> (before patch >>= \ (In  a     l) -> termIn a <$> sequenceAlt l) <|> (after patch >>= asum)
  Merge                              (In (a, _) l) -> termIn a <$> sequenceAlt l

-- | Recover the after state of a diff.
afterTerm :: (Foldable syntax, Mergeable syntax) => Diff syntax ann1 ann2 -> Maybe (Term syntax ann2)
afterTerm = cata $ \ diff -> case diff of
  Patch patch -> (after patch >>= \ (In     b  r) -> termIn b <$> sequenceAlt r) <|> (before patch >>= asum)
  Merge                             (In (_, b) r) -> termIn b <$> sequenceAlt r


-- | Strips the head annotation off a diff annotated with non-empty records.
stripDiff :: Functor syntax
          => Diff syntax (Record (h1 ': t1)) (Record (h2 ': t2))
          -> Diff syntax (Record        t1)  (Record        t2)
stripDiff = bimap rtail rtail


type instance Base (Diff syntax ann1 ann2) = DiffF syntax ann1 ann2

instance Functor syntax => Recursive   (Diff syntax ann1 ann2) where project = unDiff
instance Functor syntax => Corecursive (Diff syntax ann1 ann2) where embed = Diff


instance Eq1 syntax => Eq2 (Diff syntax) where
  liftEq2 eq1 eq2 = go where go (Diff d1) (Diff d2) = liftEq3 eq1 eq2 go d1 d2

instance (Eq1 syntax, Eq ann1, Eq ann2) => Eq (Diff syntax ann1 ann2) where
  (==) = eq2

instance Eq1 syntax => Eq3 (DiffF syntax) where
  liftEq3 eq1 eq2 eqRecur d1 d2 = case (d1, d2) of
    (Patch p1, Patch p2) -> liftEq2 (liftEq2 eq1 eqRecur) (liftEq2 eq2 eqRecur) p1 p2
    (Merge t1, Merge t2) -> liftEq2 (liftEq2 eq1 eq2) eqRecur t1 t2
    _ -> False

instance (Eq1 syntax, Eq ann1, Eq ann2) => Eq1 (DiffF syntax ann1 ann2) where
  liftEq = liftEq3 (==) (==)

instance (Eq1 syntax, Eq ann1, Eq ann2, Eq recur) => Eq (DiffF syntax ann1 ann2 recur) where
  (==) = eq3


instance Show1 syntax => Show2 (Diff syntax) where
  liftShowsPrec2 sp1 sl1 sp2 sl2 = go where go d = showsUnaryWith (liftShowsPrec3 sp1 sl1 sp2 sl2 go (showListWith (go 0))) "Diff" d . unDiff

instance (Show1 syntax, Show ann1, Show ann2) => Show (Diff syntax ann1 ann2) where
  showsPrec = showsPrec2

instance Show1 syntax => Show3 (DiffF syntax) where
  liftShowsPrec3 sp1 sl1 sp2 sl2 spRecur slRecur d diff = case diff of
    Patch patch -> showsUnaryWith (liftShowsPrec2 (liftShowsPrec2 sp1 sl1 spRecur slRecur) (liftShowList2 sp1 sl1 spRecur slRecur) (liftShowsPrec2 sp2 sl2 spRecur slRecur) (liftShowList2 sp2 sl2 spRecur slRecur)) "Patch" d patch
    Merge term -> showsUnaryWith (liftShowsPrec2 spBoth slBoth spRecur slRecur) "Merge" d term
    where spBoth = liftShowsPrec2 sp1 sl1 sp2 sl2
          slBoth = liftShowList2 sp1 sl1 sp2 sl2

instance (Show1 syntax, Show ann1, Show ann2) => Show1 (DiffF syntax ann1 ann2) where
  liftShowsPrec = liftShowsPrec3 showsPrec showList showsPrec showList

instance (Show1 syntax, Show ann1, Show ann2, Show recur) => Show (DiffF syntax ann1 ann2 recur) where
  showsPrec = showsPrec3

instance ( Named1 f, Message1 f, Named (Diff f () ()), Foldable f, Functor f) => Message (Diff f () ()) where
  encodeMessage num (Diff (Merge (In _ f))) =
    Encode.embedded num (Encode.embedded 1 (liftEncodeMessage encodeMessage 1 f))
  encodeMessage num (Diff (Patch (Delete (In _ f)))) =
    Encode.embedded num (Encode.embedded 2 (liftEncodeMessage encodeMessage 1 f))
  encodeMessage num (Diff (Patch (Insert (In _ f)))) =
    Encode.embedded num (Encode.embedded 3 (liftEncodeMessage encodeMessage 1 f))
  encodeMessage num (Diff (Patch (Replace (In _ f) (In _ g)))) =
    Encode.embedded num (Encode.embedded 4 (liftEncodeMessage encodeMessage 1 f <> liftEncodeMessage encodeMessage 2 g))
  decodeMessage num = m <|> d <|> i <|> r
    where
      m :: Decode.Parser Decode.RawMessage (Diff f () ())
      m = merge ((), ()) <$> Decode.embedded'' (Decode.embedded'' (liftDecodeMessage decodeMessage 1) `Decode.at` 1) `Decode.at` num
      d :: Decode.Parser Decode.RawMessage (Diff f () ())
      d = deleting . termIn () <$> Decode.embedded'' (Decode.embedded'' (liftDecodeMessage decodeMessage 1) `Decode.at` 2) `Decode.at` num
      i :: Decode.Parser Decode.RawMessage (Diff f () ())
      i = inserting . termIn () <$> Decode.embedded'' (Decode.embedded'' (liftDecodeMessage decodeMessage 1) `Decode.at` 3) `Decode.at` num
      r :: Decode.Parser Decode.RawMessage (Diff f () ())
      r = (\(a, b) -> replacing (termIn () a) (termIn () b)) <$>
            Decode.embedded'' (Decode.embedded'' ((,) <$> liftDecodeMessage decodeMessage 1 <*> liftDecodeMessage decodeMessage 2) `Decode.at` 4) `Decode.at` num

  dotProto (x :: Proxy (Diff f () ())) =
    [ DotProtoMessageOneOf (Single "diff")
      [ DotProtoField 1 (Prim . Named $ Single "Merge") (Single "merge") [] Nothing
      , DotProtoField 2 (Prim . Named $ Single "Delete") (Single "delete") [] Nothing
      , DotProtoField 3 (Prim . Named $ Single "Insert") (Single "insert") [] Nothing
      , DotProtoField 4 (Prim . Named $ Single "Replace") (Single "replace") [] Nothing
      ]
    , DotProtoMessageDefinition
        ( DotProtoMessage
          ( Single "Merge" )
          [ DotProtoMessageField (DotProtoField 1 (Prim . Named $ Single (nameOf1 (Proxy @f))) (Single "syntax") [] Nothing)
          , DotProtoMessageField (DotProtoField 2 (Prim . Named $ Single (nameOf x)) (Single "diff") [] Nothing)
          ] )
    , DotProtoMessageDefinition
        ( DotProtoMessage
          ( Single "Delete" )
          [ DotProtoMessageField (DotProtoField 1 (Prim . Named $ Single (nameOf1 (Proxy @f))) (Single "before") [] Nothing)
          , DotProtoMessageField (DotProtoField 2 (Prim . Named $ Single (nameOf x)) (Single "diff") [] Nothing)
          ] )
    , DotProtoMessageDefinition
        ( DotProtoMessage
          ( Single "Insert" )
          [ DotProtoMessageField (DotProtoField 1 (Prim . Named $ Single (nameOf1 (Proxy @f))) (Single "after") [] Nothing)
          , DotProtoMessageField (DotProtoField 2 (Prim . Named $ Single (nameOf x)) (Single "diff") [] Nothing)
          ] )
    , DotProtoMessageDefinition
        ( DotProtoMessage
          ( Single "Replace" )
          [ DotProtoMessageField (DotProtoField 1 (Prim . Named $ Single (nameOf1 (Proxy @f))) (Single "before") [] Nothing)
          , DotProtoMessageField (DotProtoField 2 (Prim . Named $ Single (nameOf1 (Proxy @f))) (Single "after") [] Nothing)
          , DotProtoMessageField (DotProtoField 3 (Prim . Named $ Single (nameOf x)) (Single "diff") [] Nothing)
          ] )
    ]
    -- [ DotProtoMessageField (DotProtoField 1 (Prim . Named $ Single (nameOf1 (Proxy @f))) (Single "syntax") [] Nothing) ]

-- instance (Named1 f, Message1 f, Named r) => Message (DiffF f () () r) where
--   -- encodeMessage num (Diff (Merge (In _ f))) = Encode.embedded num (liftEncodeMessage encodeMessage 1 f)
--   -- encodeMessage num (Diff (Patch (Delete (In _ f)))) = Encode.embedded num (liftEncodeMessage encodeMessage 2 f)
--   -- encodeMessage num (Diff (Patch (Insert (In _ f)))) = Encode.embedded num (liftEncodeMessage encodeMessage 3 f)
--   -- encodeMessage num (Diff (Patch (Replace (In _ f) (In _ g)))) = foldMap (Encode.embedded num . liftEncodeMessage encodeMessage 4) [f, g]
--   encodeMessage = undefined
--   decodeMessage = undefined
--   dotProto (_ :: Proxy (DiffF f () () r)) = undefined
--   -- dotProto (_ :: Proxy (DiffF f () () r)) =
--   --   [ DotProtoMessageOneOf (Single "diff")
--   --     [ DotProtoField 1 (Prim . Named $ Single "Merge") (Single "merge") [] Nothing
--   --     , DotProtoField 2 (Prim . Named $ Single "Patch") (Single "patch") [] Nothing
--   --   --   , DotProtoField 3 (Prim . Named $ Single (nameOf1 (Proxy @f))) (Single "insert") [] Nothing
--   --   --   , DotProtoField 4 (NestedRepeated . Named $ Single (nameOf1 (Proxy @f))) (Single "replace") [] Nothing
--   --     ]
--   --   -- -- , DotProtoMessageDefinition (DotProtoMessage (Single (nameOf1 (Proxy @f))) [ DotProtoMessageField (DotProtoField 1 (Prim . Named $ Single (nameOf1 (Proxy @f))) (Single "merge") [] Nothing) ])
--   --   ]
--     -- [ DotProtoMessageField (DotProtoField 1 (Prim . Named $ Single (nameOf1 (Proxy @f))) (Single "merge") [] Nothing) ]
--   -- encodeMessage num (Term (In _ f)) = Encode.embedded num (liftEncodeMessage encodeMessage 1 f)
--   -- decodeMessage num = termIn () . fromMaybe undefined <$> Decode.at (Decode.embedded (liftDecodeMessage decodeMessage 1)) num


instance Functor syntax => Bifunctor (Diff syntax) where
  bimap f g = go where go = Diff . trimap f g go . unDiff

instance Foldable syntax => Bifoldable (Diff syntax) where
  bifoldMap f g = go where go = trifoldMap f g go . unDiff

instance Traversable syntax => Bitraversable (Diff syntax) where
  bitraverse f g = go where go = fmap Diff . tritraverse f g go . unDiff


instance Functor syntax => Functor (DiffF syntax ann1 ann2) where
  fmap = trimap id id

instance Functor syntax => Trifunctor (DiffF syntax) where
  trimap f g h (Patch patch) = Patch (bimap (bimap f h) (bimap g h) patch)
  trimap f g h (Merge term)  = Merge (bimap (bimap f g) h term)

instance Foldable syntax => Foldable (DiffF syntax ann1 ann2) where
  foldMap = trifoldMap (const mempty) (const mempty)

instance Foldable syntax => Trifoldable (DiffF syntax) where
  trifoldMap f g h (Patch patch) = bifoldMap (bifoldMap f h) (bifoldMap g h) patch
  trifoldMap f g h (Merge term)  = bifoldMap (bifoldMap f g) h term

instance Traversable syntax => Traversable (DiffF syntax ann1 ann2) where
  traverse = tritraverse pure pure

instance Traversable syntax => Tritraversable (DiffF syntax) where
  tritraverse f g h (Patch patch) = Patch <$> bitraverse (bitraverse f h) (bitraverse g h) patch
  tritraverse f g h (Merge term)  = Merge <$> bitraverse (bitraverse f g) h term


instance (ToJSONFields1 syntax, ToJSONFields ann1, ToJSONFields ann2) => ToJSON (Diff syntax ann1 ann2) where
  toJSON = object . toJSONFields
  toEncoding = pairs . mconcat . toJSONFields

instance (ToJSONFields1 syntax, ToJSONFields ann1, ToJSONFields ann2) => ToJSONFields (Diff syntax ann1 ann2) where
  toJSONFields = toJSONFields . unDiff

instance (ToJSONFields1 syntax, ToJSONFields ann1, ToJSONFields ann2) => ToJSONFields1 (DiffF syntax ann1 ann2) where
  toJSONFields1 (Patch patch) = [ "patch" .= JSONFields patch ]
  toJSONFields1 (Merge term)  = [ "merge" .= JSONFields term ]

instance (ToJSONFields1 syntax, ToJSONFields ann1, ToJSONFields ann2, ToJSON recur) => ToJSONFields (DiffF syntax ann1 ann2 recur) where
  toJSONFields = toJSONFields1

instance (ToJSONFields1 syntax, ToJSONFields ann1, ToJSONFields ann2, ToJSON recur) => ToJSON (DiffF syntax ann1 ann2 recur) where
  toJSON = object . toJSONFields
  toEncoding = pairs . mconcat . toJSONFields


class Eq3 f where
  liftEq3 :: (a1 -> a2 -> Bool) -> (b1 -> b2 -> Bool) -> (c1 -> c2 -> Bool) -> f a1 b1 c1 -> f a2 b2 c2 -> Bool

eq3 :: (Eq3 f, Eq a, Eq b, Eq c) => f a b c -> f a b c -> Bool
eq3 = liftEq3 (==) (==) (==)


class Show3 f where
  liftShowsPrec3 :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS) -> (Int -> c -> ShowS) -> ([c] -> ShowS) -> Int -> f a b c -> ShowS

showsPrec3 :: (Show3 f, Show a, Show b, Show c) => Int -> f a b c -> ShowS
showsPrec3 = liftShowsPrec3 showsPrec showList showsPrec showList showsPrec showList

class Trifunctor f where
  trimap :: (a -> a') -> (b -> b') -> (c -> c') -> f a b c -> f a' b' c'

class Trifoldable f where
  trifoldMap :: Monoid m => (a -> m) -> (b -> m) -> (c -> m) -> f a b c -> m

class Tritraversable f where
  tritraverse :: Applicative g => (a -> g a') -> (b -> g b') -> (c -> g c') -> f a b c -> g (f a' b' c')
