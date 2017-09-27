{-# LANGUAGE DataKinds, DefaultSignatures, GADTs, RankNTypes, TypeOperators, UndecidableInstances #-}
module Algorithm where

import Control.Applicative (Alternative(..))
import Control.Monad (guard)
import Control.Monad.Free.Freer
import Data.Functor.Classes
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Proxy
import Data.Term
import Data.These
import Data.Union
import Diff
import GHC.Generics

-- | A single step in a diffing algorithm, parameterized by the types of terms, diffs, and the result of the applicable algorithm.
data AlgorithmF term1 term2 result partial where
  -- | Diff two terms with the choice of algorithm left to the interpreter’s discretion.
  Diff :: term1 -> term2 -> AlgorithmF term1 term2 result result
  -- | Diff two terms recursively in O(n) time, resulting in a single diff node.
  Linear :: term1 -> term2 -> AlgorithmF term1 term2 result result
  -- | Diff two lists of terms by each element’s similarity in O(n³ log n), resulting in a list of diffs.
  RWS :: [term1] -> [term2] -> AlgorithmF term1 term2 result [result]
  -- | Delete a term.
  Delete :: term1 -> AlgorithmF term1 term2 result result
  -- | Insert a term.
  Insert :: term2 -> AlgorithmF term1 term2 result result
  -- | Replace one term with another.
  Replace :: term1 -> term2 -> AlgorithmF term1 term2 result result
  -- | An 'Algorithm' that always fails.
  Empty :: AlgorithmF term1 term2 result a
  -- | An 'Algorithm' to try one of two alternatives.
  Alt :: a -> a -> AlgorithmF term1 term2 result a

-- | The free(r) monad for 'AlgorithmF'. This enables us to construct algorithms to diff using '<$>', '<*>', '>>=', and do-notation.
type Algorithm term1 term2 result = Freer (AlgorithmF term1 term2 result)


-- DSL

-- | Diff two terms without specifying the algorithm to be used.
diff :: term1 -> term2 -> Algorithm term1 term2 result result
diff = (liftF .) . Algorithm.Diff

-- | Diff a These of terms without specifying the algorithm to be used.
diffThese :: These term1 term2 -> Algorithm term1 term2 result result
diffThese = these byDeleting byInserting diff

-- | Diff a pair of optional terms without specifying the algorithm to be used.
diffMaybe :: Maybe term1 -> Maybe term2 -> Algorithm term1 term2 result (Maybe result)
diffMaybe (Just a) (Just b) = Just <$> diff a b
diffMaybe (Just a) _        = Just <$> byDeleting a
diffMaybe _        (Just b) = Just <$> byInserting b
diffMaybe _        _        = pure Nothing

-- | Diff two terms linearly.
linearly :: term1 -> term2 -> Algorithm term1 term2 result result
linearly f1 f2 = liftF (Linear f1 f2)

-- | Diff two terms using RWS.
byRWS :: [term1] -> [term2] -> Algorithm term1 term2 result [result]
byRWS a b = liftF (RWS a b)

-- | Delete a term.
byDeleting :: term1 -> Algorithm term1 term2 result result
byDeleting = liftF . Delete

-- | Insert a term.
byInserting :: term2 -> Algorithm term1 term2 result result
byInserting = liftF . Insert

-- | Replace one term with another.
byReplacing :: term1 -> term2 -> Algorithm term1 term2 result result
byReplacing = (liftF .) . Replace


instance (Show term1, Show term2) => Show1 (AlgorithmF term1 term2 result) where
  liftShowsPrec sp _ d algorithm = case algorithm of
    Algorithm.Diff t1 t2 -> showsBinaryWith showsPrec showsPrec "Diff" d t1 t2
    Linear t1 t2 -> showsBinaryWith showsPrec showsPrec "Linear" d t1 t2
    RWS as bs -> showsBinaryWith (liftShowsPrec showsPrec showList) (liftShowsPrec showsPrec showList) "RWS" d as bs
    Delete t1 -> showsUnaryWith showsPrec "Delete" d t1
    Insert t2 -> showsUnaryWith showsPrec "Insert" d t2
    Replace t1 t2 -> showsBinaryWith showsPrec showsPrec "Replace" d t1 t2
    Empty -> showString "Empty"
    Alt a b -> showsBinaryWith sp sp "Alt" d a b


instance Alternative (Algorithm term1 term2 result) where
  empty = Empty `Then` return

  (Empty `Then` _) <|> b = b
  a <|> (Empty `Then` _) = a
  a <|> b = Alt a b `Then` id


-- | Diff two terms based on their generic Diffable instances. If the terms are not diffable
-- (represented by a Nothing diff returned from algorithmFor) replace one term with another.
algorithmForTerms :: Diffable syntax
                  => Term syntax ann1
                  -> Term syntax ann2
                  -> Algorithm (Term syntax ann1) (Term syntax ann2) (Diff syntax ann1 ann2) (Diff syntax ann1 ann2)
algorithmForTerms t1@(Term (In ann1 f1)) t2@(Term (In ann2 f2))
  =   mergeFor t1 t2
  <|> deleteF . In ann1      <$> subalgorithmFor byDeleting  (flip mergeFor t2) f1
  <|> insertF . In      ann2 <$> subalgorithmFor byInserting (     mergeFor t1) f2
  where mergeFor (Term (In ann1 f1)) (Term (In ann2 f2)) = merge (ann1, ann2) <$> algorithmFor f1 f2

-- | A type class for determining what algorithm to use for diffing two terms.
class Diffable f where
  -- | Construct an algorithm to diff a pair of @f@s populated with disjoint terms.
  algorithmFor :: f term1
               -> f term2
               -> Algorithm term1 term2 result (f result)
  default
    algorithmFor :: (Generic1 f, GDiffable (Rep1 f))
                 => f term1
                 -> f term2
                 -> Algorithm term1 term2 result (f result)
  algorithmFor = genericAlgorithmFor

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
  subalgorithmFor :: Alternative g -- ^ The 'Alternative' instance will in general be 'Algorithm', but left opaque to make it harder to shoot oneself in the foot.
                  => (a -> g b)    -- ^ A “blur” function to traverse positions which should not be diffed against.
                  -> (a -> g b)    -- ^ A “focus” function to traverse positions which should be diffed against.
                  -> f a           -- ^ The syntax to diff inside of.
                  -> g (f b)       -- ^ The resulting algorithm (or other 'Alternative' context), producing the traversed syntax.
  subalgorithmFor _ _ _ = empty

genericAlgorithmFor :: (Generic1 f, GDiffable (Rep1 f))
                    => f term1
                    -> f term2
                    -> Algorithm term1 term2 result (f result)
genericAlgorithmFor a b = to1 <$> galgorithmFor (from1 a) (from1 b)


-- | Diff a Union of Syntax terms. Left is the "rest" of the Syntax terms in the Union,
-- Right is the "head" of the Union. 'weaken' relaxes the Union to allow the possible
-- diff terms from the "rest" of the Union, and 'inj' adds the diff terms into the Union.
-- NB: If Left or Right Syntax terms in our Union don't match, we fail fast by returning Nothing.
instance Apply Diffable fs => Diffable (Union fs) where
  algorithmFor u1 u2 = fromMaybe empty (apply2' (Proxy :: Proxy Diffable) (\ inj f1 f2 -> inj <$> algorithmFor f1 f2) u1 u2)

  subalgorithmFor blur focus = apply' (Proxy :: Proxy Diffable) (\ inj f1 -> inj <$> subalgorithmFor blur focus f1)

-- | Diff two 'Maybe's.
instance Diffable Maybe where
  algorithmFor = diffMaybe

-- | Diff two lists using RWS.
instance Diffable [] where
  algorithmFor = byRWS

-- | Diff two non-empty lists using RWS.
instance Diffable NonEmpty where
  algorithmFor (a:|as) (b:|bs) = (\ (d:ds) -> d:|ds) <$> byRWS (a:as) (b:bs)

-- | A generic type class for diffing two terms defined by the Generic1 interface.
class GDiffable f where
  galgorithmFor :: f term1 -> f term2 -> Algorithm term1 term2 result (f result)

-- | Diff two constructors (M1 is the Generic1 newtype for meta-information (possibly related to type constructors, record selectors, and data types))
instance GDiffable f => GDiffable (M1 i c f) where
  galgorithmFor (M1 a) (M1 b) = M1 <$> galgorithmFor a b

-- | Diff the fields of a product type.
-- i.e. data Foo a b = Foo a b (the 'Foo a b' is captured by 'a :*: b').
instance (GDiffable f, GDiffable g) => GDiffable (f :*: g) where
  galgorithmFor (a1 :*: b1) (a2 :*: b2) = (:*:) <$> galgorithmFor a1 a2 <*> galgorithmFor b1 b2

-- | Diff the constructors of a sum type.
-- i.e. data Foo a = Foo a | Bar a (the 'Foo a' is captured by L1 and 'Bar a' is R1).
instance (GDiffable f, GDiffable g) => GDiffable (f :+: g) where
  galgorithmFor (L1 a) (L1 b) = L1 <$> galgorithmFor a b
  galgorithmFor (R1 a) (R1 b) = R1 <$> galgorithmFor a b
  galgorithmFor _ _ = empty

-- | Diff two parameters (Par1 is the Generic1 newtype representing a type parameter).
-- i.e. data Foo a = Foo a (the 'a' is captured by Par1).
instance GDiffable Par1 where
  galgorithmFor (Par1 a) (Par1 b) = Par1 <$> diff a b

-- | Diff two constant parameters (K1 is the Generic1 newtype representing type parameter constants).
-- i.e. data Foo = Foo Int (the 'Int' is a constant parameter).
instance Eq c => GDiffable (K1 i c) where
  galgorithmFor (K1 a) (K1 b) = guard (a == b) *> pure (K1 a)

-- | Diff two terms whose constructors contain 0 type parameters.
-- i.e. data Foo = Foo.
instance GDiffable U1 where
  galgorithmFor _ _ = pure U1

-- | Diff two 'Diffable' containers of parameters.
instance Diffable f => GDiffable (Rec1 f) where
  galgorithmFor a b = Rec1 <$> algorithmFor (unRec1 a) (unRec1 b)
