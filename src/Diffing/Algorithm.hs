{-# LANGUAGE DefaultSignatures, DeriveFunctor, DeriveGeneric, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, TypeApplications, TypeOperators, UndecidableInstances #-}
module Diffing.Algorithm
  ( Diff (..)
  , Algorithm(..)
  , Diffable (..)
  , Equivalence (..)
  , diff
  , diffEdit
  , diffMaybe
  , linearly
  , byReplacing
  , comparableTerms
  , equivalentTerms
  , algorithmForTerms
  ) where

import Control.Effect.Carrier hiding ((:+:))
import Control.Effect.NonDet
import qualified Data.Diff as Diff
import qualified Data.Edit as Edit
import Data.Sum
import Data.Term
import GHC.Generics
import Prologue

-- | A single step in a diffing algorithm, parameterized by the types of terms, diffs, and the result of the applicable algorithm.
data Diff term1 term2 diff (m :: * -> *) k
  -- | Diff two terms with the choice of algorithm left to the interpreter’s discretion.
  = Diff term1 term2 (diff -> m k)
  -- | Diff two terms recursively in O(n) time, resulting in a single diff node.
  | Linear term1 term2 (diff -> m k)
  -- | Diff two lists of terms by each element’s similarity in O(n³ log n), resulting in a list of diffs.
  | RWS [term1] [term2] ([diff] -> m k)
  -- | Delete a term.
  | Delete term1 (diff -> m k)
  -- | Insert a term.
  | Insert term2 (diff -> m k)
  -- | Replace one term with another.
  | Replace term1 term2 (diff -> m k)
  deriving (Functor, Generic1)

instance HFunctor (Diff term1 term2 diff)
instance Effect   (Diff term1 term2 diff)

newtype Algorithm term1 term2 diff m a = Algorithm { runAlgorithm :: m a }
  deriving (Applicative, Alternative, Functor, Monad)

instance Carrier sig m => Carrier sig (Algorithm term1 term2 diff m) where
  eff = Algorithm . eff . handleCoercible


-- DSL

-- | Diff two terms without specifying the algorithm to be used.
diff :: (Carrier sig m, Member (Diff term1 term2 diff) sig) => term1 -> term2 -> m diff
diff a1 a2 = send (Diff a1 a2 pure)

-- | Diff an 'Edit.Edit' of terms without specifying the algorithm to be used.
diffEdit :: (Carrier sig m, Member (Diff term1 term2 diff) sig) => Edit.Edit term1 term2 -> Algorithm term1 term2 diff m diff
diffEdit = Edit.edit byDeleting byInserting diff

-- | Diff a pair of optional terms without specifying the algorithm to be used.
diffMaybe :: (Carrier sig m, Member (Diff term1 term2 diff) sig) => Maybe term1 -> Maybe term2 -> Algorithm term1 term2 diff m (Maybe diff)
diffMaybe (Just a1) (Just a2) = Just <$> diff a1 a2
diffMaybe (Just a1) _         = Just <$> byDeleting a1
diffMaybe _         (Just a2) = Just <$> byInserting a2
diffMaybe _         _         = pure Nothing

-- | Diff two terms linearly.
linearly :: (Carrier sig m, Member (Diff term1 term2 diff) sig) => term1 -> term2 -> Algorithm term1 term2 diff m diff
linearly f1 f2 = send (Linear f1 f2 pure)

-- | Diff two terms using RWS.
byRWS :: (Carrier sig m, Member (Diff term1 term2 diff) sig) => [term1] -> [term2] -> Algorithm term1 term2 diff m [diff]
byRWS as1 as2 = send (RWS as1 as2 pure)

-- | Delete a term.
byDeleting :: (Carrier sig m, Member (Diff term1 term2 diff) sig) => term1 -> Algorithm term1 term2 diff m diff
byDeleting a1 = sendDiff (Delete a1 pure)

-- | Insert a term.
byInserting :: (Carrier sig m, Member (Diff term1 term2 diff) sig) => term2 -> Algorithm term1 term2 diff m diff
byInserting a2 = sendDiff (Insert a2 pure)

-- | Replace one term with another.
byReplacing :: (Carrier sig m, Member (Diff term1 term2 diff) sig) => term1 -> term2 -> Algorithm term1 term2 diff m diff
byReplacing a1 a2 = send (Replace a1 a2 pure)

sendDiff :: (Carrier sig m, Member (Diff term1 term2 diff) sig) => Diff term1 term2 diff m a -> Algorithm term1 term2 diff m a
sendDiff = Algorithm . send


-- | Diff two terms based on their 'Diffable' instances, performing substructural comparisons iff the initial comparison fails.
algorithmForTerms :: (Carrier sig m, Diffable syntax, Member (Diff (Term syntax ann1) (Term syntax ann2) (Diff.Diff syntax ann1 ann2)) sig, Member NonDet sig, Alternative m)
                  => Term syntax ann1
                  -> Term syntax ann2
                  -> Algorithm (Term syntax ann1) (Term syntax ann2) (Diff.Diff syntax ann1 ann2) m (Diff.Diff syntax ann1 ann2)
algorithmForTerms t1@(Term (In ann1 f1)) t2@(Term (In ann2 f2))
  =   mergeFor t1 t2
  <|> Diff.deleteF . In ann1      <$> subalgorithmFor byDeleting  (`mergeFor` t2) f1
  <|> Diff.insertF . In      ann2 <$> subalgorithmFor byInserting (mergeFor t1) f2
  where mergeFor (Term (In ann1 f1)) (Term (In ann2 f2)) = Diff.merge (ann1, ann2) <$> algorithmFor f1 f2

-- | An O(1) relation on terms indicating their non-recursive comparability (i.e. are they of the same “kind” in a way that warrants comparison), defined in terms of the comparability of their respective syntax.
comparableTerms :: Diffable syntax
                => TermF syntax ann1 term1
                -> TermF syntax ann2 term2
                -> Bool
comparableTerms (In _ syntax1) (In _ syntax2) = comparableTo syntax1 syntax2

-- | An O(n) relation on terms indicating their recursive equivalence (i.e. are they _notionally_ “the same,” as distinct from literal equality), defined at each node in terms of the equivalence of their respective syntax, computed first on a nominated subterm (if any), falling back to substructural equivalence (e.g. equivalence of one term against the subject of the other, annotating term), and finally to equality.
equivalentTerms :: (Diffable syntax, Eq1 syntax)
                => Term syntax ann1
                -> Term syntax ann2
                -> Bool
equivalentTerms term1@(Term (In _ syntax1)) term2@(Term (In _ syntax2))
  =  fromMaybe False (equivalentTerms <$> equivalentBySubterm syntax1 <*> equivalentBySubterm syntax2)
  || runEquivalence (subalgorithmFor pure (Equivalence . flip equivalentTerms term2) syntax1)
  || runEquivalence (subalgorithmFor pure (Equivalence .      equivalentTerms term1) syntax2)
  || liftEq equivalentTerms syntax1 syntax2

-- | A constant 'Alternative' functor used by 'equivalentTerms' to compute the substructural equivalence of syntax.
newtype Equivalence a = Equivalence { runEquivalence :: Bool }
  deriving (Eq, Functor)

instance Applicative Equivalence where
  pure _ = Equivalence True
  Equivalence a <*> Equivalence b = Equivalence (a && b)

instance Alternative Equivalence where
  empty = Equivalence False
  Equivalence a <|> Equivalence b = Equivalence (a || b)

-- | A type class for determining what algorithm to use for diffing two terms.
class Diffable f where
  -- | Construct an algorithm to diff a pair of @f@s populated with disjoint terms.
  algorithmFor :: (Alternative m, Carrier sig m, Member (Diff term1 term2 diff) sig, Member NonDet sig)
               => f term1
               -> f term2
               -> Algorithm term1 term2 diff m (f diff)
  default
    algorithmFor :: (Alternative m, Carrier sig m, Generic1 f, GDiffable (Rep1 f), Member (Diff term1 term2 diff) sig, Member NonDet sig)
                 => f term1
                 -> f term2
                 -> Algorithm term1 term2 diff m (f diff)
  algorithmFor = genericAlgorithmFor

  tryAlignWith :: Alternative g => (Edit.Edit a1 a2 -> g b) -> f a1 -> f a2 -> g (f b)
  default tryAlignWith :: (Alternative g, Generic1 f, GDiffable (Rep1 f)) => (Edit.Edit a1 a2 -> g b) -> f a1 -> f a2 -> g (f b)
  tryAlignWith f a b = to1 <$> gtryAlignWith f (from1 a) (from1 b)

  -- | Construct an algorithm to diff against positions inside an @f@.
  --
  --   This is very like 'traverse', with two key differences:
  --
  --   1. The traversal distributes through an 'Alternative' functor, not just an 'Applicative'.
  --   2. The traversal is mediated by two different functions, one for positions which should be ignored for substructural diffing, the other for positions which should be diffed substructurally.
  --
  --   These two functions allow us to say e.g. that comparisons against 'Data.Syntax.Context' should also be made against its subject, but not against any of the comments, resulting in the insertion of both comments and context when documenting an existing function.
  --
  --   By default, 'subalgorithmFor' produces 'empty', rejecting substructural comparisons. This is important for performance, as alternations with 'empty' are eliminated at construction time.
  -- ^ The 'Alternative' instance will in general be 'Diff', but left opaque to make it harder to shoot oneself in the foot.
  subalgorithmFor :: Alternative g
                  => (a -> g b)    -- ^ A “blur” function to traverse positions which should not be diffed against.
                  -> (a -> g b)    -- ^ A “focus” function to traverse positions which should be diffed against.
                  -> f a           -- ^ The syntax to diff inside of.
                  -> g (f b)       -- ^ The resulting algorithm (or other 'Alternative' context), producing the traversed syntax.
  subalgorithmFor _ _ _ = empty

  -- | Syntax having a human-provided identifier, such as function/method definitions, can use equivalence of identifiers as a proxy for their overall equivalence, improving the quality & efficiency of the diff as a whole.
  --
  --   This can also be used for annotation nodes to ensure that their subjects’ equivalence is weighed appropriately.
  --
  --   Other syntax should use the default definition, and thus have equivalence computed piece-wise.
  equivalentBySubterm :: f a -> Maybe a
  equivalentBySubterm _ = Nothing

  -- | A relation on syntax values indicating their  In general this should be true iff both values have the same constructor (this is the relation computed by the default, generic definition).
  --
  --   For syntax with constant fields which serve as a classifier, this method can be overloaded to consider equality on that classifier in addition to/instead of the constructors themselves, and thus limit the comparisons accordingly.
  comparableTo :: f term1 -> f term2 -> Bool
  default comparableTo :: (Generic1 f, GDiffable (Rep1 f)) => f term1 -> f term2 -> Bool
  comparableTo = genericComparableTo

genericAlgorithmFor :: (Alternative m, Carrier sig m, Generic1 f, GDiffable (Rep1 f), Member (Diff term1 term2 diff) sig, Member NonDet sig)
                    => f term1
                    -> f term2
                    -> Algorithm term1 term2 diff m (f diff)
genericAlgorithmFor a1 a2 = to1 <$> galgorithmFor (from1 a1) (from1 a2)

genericComparableTo :: (Generic1 f, GDiffable (Rep1 f)) => f term1 -> f term2 -> Bool
genericComparableTo a1 a2 = gcomparableTo (from1 a1) (from1 a2)


-- | 'Diffable' for 'Sum's of syntax functors is defined in general by straightforward lifting of each method into the functors in the 'Sum'.
instance Apply Diffable fs => Diffable (Sum fs) where
  algorithmFor u1 u2 = fromMaybe empty (apply2' @Diffable (\ inj f1 f2 -> inj <$> algorithmFor f1 f2) u1 u2)

  tryAlignWith f u1 u2 = fromMaybe empty (apply2' @Diffable (\ inj t1 t2 -> inj <$> tryAlignWith f t1 t2) u1 u2)

  subalgorithmFor blur focus = apply' @Diffable (\ inj f -> inj <$> subalgorithmFor blur focus f)

  equivalentBySubterm = apply @Diffable equivalentBySubterm

  -- | Comparability on 'Sum's is defined first by comparability of their contained functors (when they’re the same), falling back to using 'subalgorithmFor' to opt substructurally-diffable syntax into comparisons (e.g. to allow annotating nodes to be compared against the kind of nodes they annotate).
  comparableTo u1 u2 = fromMaybe False (apply2 @Diffable comparableTo u1 u2 <|> True <$ subalgorithmFor pure pure u1 <|> True <$ subalgorithmFor pure pure u2)

-- | Diff two 'Maybe's.
instance Diffable Maybe where
  algorithmFor = diffMaybe

  tryAlignWith f (Just a1) (Just a2) = Just <$> f (Edit.Compare a1 a2)
  tryAlignWith f (Just a1) Nothing   = Just <$> f (Edit.Delete a1)
  tryAlignWith f Nothing   (Just a2) = Just <$> f (Edit.Insert a2)
  tryAlignWith _ Nothing   Nothing   = pure Nothing

-- | Diff two lists using RWS.
instance Diffable [] where
  algorithmFor = byRWS

  tryAlignWith f (a1:as1) (a2:as2) = (:) <$> f (Edit.Compare a1 a2) <*> tryAlignWith f as1 as2
  tryAlignWith f []       as2      = traverse (f . Edit.Insert) as2
  tryAlignWith f as1      []       = traverse (f . Edit.Delete) as1

-- | Diff two non-empty lists using RWS.
instance Diffable NonEmpty where
  algorithmFor (a1:|as1) (a2:|as2) = nonEmpty <$> byRWS (a1:as1) (a2:as2) >>= maybeM empty

  tryAlignWith f (a1:|as1) (a2:|as2) = (:|) <$> f (Edit.Compare a1 a2) <*> tryAlignWith f as1 as2

-- | A generic type class for diffing two terms defined by the Generic1 interface.
class GDiffable f where
  galgorithmFor :: (Alternative m, Carrier sig m, Member (Diff term1 term2 diff) sig, Member NonDet sig) => f term1 -> f term2 -> Algorithm term1 term2 diff m (f diff)

  gtryAlignWith :: Alternative g => (Edit.Edit a1 a2 -> g b) -> f a1 -> f a2 -> g (f b)

  gcomparableTo :: f term1 -> f term2 -> Bool
  gcomparableTo _ _ = True

-- | Diff two constructors (M1 is the Generic1 newtype for meta-information (possibly related to type constructors, record selectors, and data types))
instance GDiffable f => GDiffable (M1 i c f) where
  galgorithmFor (M1 a1) (M1 a2) = M1 <$> galgorithmFor a1 a2

  gtryAlignWith f (M1 a) (M1 b) = M1 <$> gtryAlignWith f a b

  gcomparableTo (M1 a1) (M1 a2) = gcomparableTo a1 a2

-- | Diff the fields of a product type.
-- i.e. data Foo a b = Foo a b (the 'Foo a b' is captured by 'a :*: b').
instance (GDiffable f, GDiffable g) => GDiffable (f :*: g) where
  galgorithmFor (a1 :*: b1) (a2 :*: b2) = (:*:) <$> galgorithmFor a1 a2 <*> galgorithmFor b1 b2

  gtryAlignWith f (a1 :*: b1) (a2 :*: b2) = (:*:) <$> gtryAlignWith f a1 a2 <*> gtryAlignWith f b1 b2

-- | Diff the constructors of a sum type.
-- i.e. data Foo a = Foo a | Bar a (the 'Foo a' is captured by L1 and 'Bar a' is R1).
instance (GDiffable f, GDiffable g) => GDiffable (f :+: g) where
  galgorithmFor (L1 a1) (L1 a2) = L1 <$> galgorithmFor a1 a2
  galgorithmFor (R1 b1) (R1 b2) = R1 <$> galgorithmFor b1 b2
  galgorithmFor _ _ = empty

  gtryAlignWith f a b = case (a, b) of
    (L1 a, L1 b) -> L1 <$> gtryAlignWith f a b
    (R1 a, R1 b) -> R1 <$> gtryAlignWith f a b
    _ -> empty

  gcomparableTo (L1 _) (L1 _) = True
  gcomparableTo (R1 _) (R1 _) = True
  gcomparableTo _      _      = False

-- | Diff two parameters (Par1 is the Generic1 newtype representing a type parameter).
-- i.e. data Foo a = Foo a (the 'a' is captured by Par1).
instance GDiffable Par1 where
  galgorithmFor (Par1 a1) (Par1 a2) = Par1 <$> diff a1 a2

  gtryAlignWith f (Par1 a) (Par1 b) = Par1 <$> f (Edit.Compare a b)

-- | Diff two constant parameters (K1 is the Generic1 newtype representing type parameter constants).
-- i.e. data Foo = Foo Int (the 'Int' is a constant parameter).
instance Eq c => GDiffable (K1 i c) where
  galgorithmFor (K1 a1) (K1 a2) = guard (a1 == a2) $> K1 a1

  gtryAlignWith _ (K1 a) (K1 b) = guard (a == b) $> K1 b

-- | Diff two terms whose constructors contain 0 type parameters.
-- i.e. data Foo = Foo.
instance GDiffable U1 where
  galgorithmFor _ _ = pure U1

  gtryAlignWith _ _ _ = pure U1

-- | Diff two 'Diffable' containers of parameters.
instance Diffable f => GDiffable (Rec1 f) where
  galgorithmFor a1 a2 = Rec1 <$> algorithmFor (unRec1 a1) (unRec1 a2)

  gtryAlignWith f (Rec1 a) (Rec1 b) = Rec1 <$> tryAlignWith f a b
