{-# LANGUAGE DataKinds, DefaultSignatures, GADTs, RankNTypes, TypeOperators, UndecidableInstances #-}
module Algorithm where

import Control.Applicative (Alternative(..))
import Control.Monad (guard)
import Control.Monad.Free.Freer
import Data.Functor.Classes
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Proxy
import Data.These
import Data.Union
import Diff
import GHC.Generics
import Term

-- | A single step in a diffing algorithm, parameterized by the types of terms, diffs, and the result of the applicable algorithm.
data AlgorithmF term diff result where
  -- | Diff two terms with the choice of algorithm left to the interpreter’s discretion.
  Diff :: term ann1 -> term ann2 -> AlgorithmF term (diff ann1 ann2) (diff ann1 ann2)
  -- | Diff two terms recursively in O(n) time, resulting in a single diff node.
  Linear :: term ann1 -> term ann2 -> AlgorithmF term (diff ann1 ann2) (diff ann1 ann2)
  -- | Diff two lists of terms by each element’s similarity in O(n³ log n), resulting in a list of diffs.
  RWS :: [term ann1] -> [term ann2] -> AlgorithmF term (diff ann1 ann2) [diff ann1 ann2]
  -- | Delete a term.
  Delete :: term ann1 -> AlgorithmF term (diff ann1 ann2) (diff ann1 ann2)
  -- | Insert a term.
  Insert :: term ann2 -> AlgorithmF term (diff ann1 ann2) (diff ann1 ann2)
  -- | Replace one term with another.
  Replace :: term ann1 -> term ann2 -> AlgorithmF term (diff ann1 ann2) (diff ann1 ann2)
  -- | An 'Algorithm' that always fails.
  Empty :: AlgorithmF term diff a
  -- | An 'Algorithm' to try one of two alternatives.
  Alt :: a -> a -> AlgorithmF term diff a

-- | The free(r) monad for 'AlgorithmF'. This enables us to construct algorithms to diff using '<$>', '<*>', '>>=', and do-notation.
type Algorithm term diff = Freer (AlgorithmF term diff)


-- DSL

-- | Diff two terms without specifying the algorithm to be used.
diff :: term ann1 -> term ann2 -> Algorithm term (diff ann1 ann2) (diff ann1 ann2)
diff = (liftF .) . Algorithm.Diff

-- | Diff a These of terms without specifying the algorithm to be used.
diffThese :: These (term ann1) (term ann2) -> Algorithm term (diff ann1 ann2) (diff ann1 ann2)
diffThese = these byDeleting byInserting diff

-- | Diff a pair of optional terms without specifying the algorithm to be used.
diffMaybe :: Maybe (term ann1) -> Maybe (term ann2) -> Algorithm term (diff ann1 ann2) (Maybe (diff ann1 ann2))
diffMaybe a b = case (a, b) of
  (Just a, Just b) -> Just <$> diff a b
  (Just a, _) -> Just <$> byDeleting a
  (_, Just b) -> Just <$> byInserting b
  _ -> pure Nothing

-- | Diff two terms linearly.
linearly :: term ann1 -> term ann2 -> Algorithm term (diff ann1 ann2) (diff ann1 ann2)
linearly a b = liftF (Linear a b)

-- | Diff two terms using RWS.
byRWS :: [term ann1] -> [term ann2] -> Algorithm term (diff ann1 ann2) [diff ann1 ann2]
byRWS a b = liftF (RWS a b)

-- | Delete a term.
byDeleting :: term ann1 -> Algorithm term (diff ann1 ann2) (diff ann1 ann2)
byDeleting = liftF . Delete

-- | Insert a term.
byInserting :: term ann2 -> Algorithm term (diff ann1 ann2) (diff ann1 ann2)
byInserting = liftF . Insert

-- | Replace one term with another.
byReplacing :: term ann1 -> term ann2 -> Algorithm term (diff ann1 ann2) (diff ann1 ann2)
byReplacing = (liftF .) . Replace


instance (Show1 term, Show ann1, Show ann2) => Show1 (AlgorithmF term (diff ann1 ann2)) where
  liftShowsPrec sp _ d algorithm = case algorithm of
    Algorithm.Diff t1 t2 -> showsBinaryWith showsTerm showsTerm "Diff" d t1 t2
    Linear t1 t2 -> showsBinaryWith showsTerm showsTerm "Linear" d t1 t2
    RWS as bs -> showsBinaryWith (liftShowsPrec showsTerm (liftShowList showsPrec showList)) (liftShowsPrec showsTerm (liftShowList showsPrec showList)) "RWS" d as bs
    Delete t1 -> showsUnaryWith showsTerm "Delete" d t1
    Insert t2 -> showsUnaryWith showsTerm "Insert" d t2
    Replace t1 t2 -> showsBinaryWith showsTerm showsTerm "Replace" d t1 t2
    Empty -> showString "Empty"
    Alt a b -> showsBinaryWith sp sp "Alt" d a b
    where showsTerm :: (Show1 term, Show ann) => Int -> term ann -> ShowS
          showsTerm = liftShowsPrec showsPrec showList


instance Alternative (Algorithm term diff) where
  empty = Empty `Then` return

  (Empty `Then` _) <|> b = b
  a <|> (Empty `Then` _) = a
  a <|> b = Alt a b `Then` id


-- | Diff two terms based on their generic Diffable instances. If the terms are not diffable
-- (represented by a Nothing diff returned from algorithmFor) replace one term with another.
algorithmForTerms :: Diffable syntax
                  => Term syntax ann1
                  -> Term syntax ann2
                  -> Algorithm (Term syntax) (Diff syntax ann1 ann2) (Diff syntax ann1 ann2)
algorithmForTerms (Term (In ann1 f1)) (Term (In ann2 f2)) = merge (ann1, ann2) <$> algorithmFor f1 f2


-- | A type class for determining what algorithm to use for diffing two terms.
class Diffable f where
  algorithmFor :: f (term ann1)
               -> f (term ann2)
               -> Algorithm term (diff ann1 ann2) (f (diff ann1 ann2))
  default
    algorithmFor :: (Generic1 f, GDiffable (Rep1 f))
                 => f (term ann1)
                 -> f (term ann2)
                 -> Algorithm term (diff ann1 ann2) (f (diff ann1 ann2))
  algorithmFor = genericAlgorithmFor

genericAlgorithmFor :: (Generic1 f, GDiffable (Rep1 f)) => f (term ann1) -> f (term ann2) -> Algorithm term (diff ann1 ann2) (f (diff ann1 ann2))
genericAlgorithmFor a b = to1 <$> galgorithmFor (from1 a) (from1 b)


-- | Diff a Union of Syntax terms. Left is the "rest" of the Syntax terms in the Union,
-- Right is the "head" of the Union. 'weaken' relaxes the Union to allow the possible
-- diff terms from the "rest" of the Union, and 'inj' adds the diff terms into the Union.
-- NB: If Left or Right Syntax terms in our Union don't match, we fail fast by returning Nothing.
instance Apply Diffable fs => Diffable (Union fs) where
  algorithmFor u1 u2 = fromMaybe empty (apply2' (Proxy :: Proxy Diffable) (\ inj f1 f2 -> inj <$> algorithmFor f1 f2) u1 u2)

-- | Diff two lists using RWS.
instance Diffable [] where
  algorithmFor a b = byRWS a b

-- | Diff two non-empty lists using RWS.
instance Diffable NonEmpty where
  algorithmFor (a:|as) (b:|bs) = (\ (d:ds) -> d:|ds) <$> byRWS (a:as) (b:bs)

-- | A generic type class for diffing two terms defined by the Generic1 interface.
class GDiffable f where
  galgorithmFor :: f (term ann1) -> f (term ann2) -> Algorithm term (diff ann1 ann2) (f (diff ann1 ann2))

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
  galgorithmFor (Par1 a) (Par1 b) = Par1 <$> linearly a b

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
