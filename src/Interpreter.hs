{-# LANGUAGE RankNTypes #-}
module Interpreter (Comparable, DiffConstructor, diffTerms) where

import Algorithm
import Data.Align.Generic
import Data.Functor.Foldable
import Data.Functor.Both
import Data.RandomWalkSimilarity
import Data.Record
import Data.These
import qualified Data.Vector as Vector
import Diff
import Info
import Patch
import Prologue hiding (lookup)
import SES
import Syntax as S
import Term

-- | Returns whether two terms are comparable
type Comparable f annotation = Term f annotation -> Term f annotation -> Bool

-- | Constructs a diff from the CofreeF containing its annotation and syntax. This function has the opportunity to, for example, cache properties in the annotation.
type DiffConstructor f annotation = TermF f (Both annotation) (Diff f annotation) -> Diff f annotation

-- | Diff two terms recursively, given functions characterizing the diffing.
diffTerms :: (Eq leaf, HasField fields Category, HasField fields (Vector.Vector Double))
  => DiffConstructor (Syntax leaf) (Record fields) -- ^ A function to wrap up & possibly annotate every produced diff.
  -> Comparable (Syntax leaf) (Record fields) -- ^ A function to determine whether or not two terms should even be compared.
  -> SES.Cost (SyntaxDiff leaf fields) -- ^ A function to compute the cost of a given diff node.
  -> SyntaxTerm leaf fields -- ^ A term representing the old state.
  -> SyntaxTerm leaf fields -- ^ A term representing the new state.
  -> SyntaxDiff leaf fields
diffTerms construct comparable cost a b = fromMaybe (replacing a b) $ diffComparableTerms construct comparable cost a b

-- | Diff two terms recursively, given functions characterizing the diffing. If the terms are incomparable, returns 'Nothing'.
diffComparableTerms :: (Eq leaf, HasField fields Category, HasField fields (Vector.Vector Double)) => DiffConstructor (Syntax leaf) (Record fields) -> Comparable (Syntax leaf) (Record fields) -> SES.Cost (SyntaxDiff leaf fields) -> SyntaxTerm leaf fields -> SyntaxTerm leaf fields -> Maybe (SyntaxDiff leaf fields)
diffComparableTerms construct comparable cost = recur
  where recur a b
          | (category <$> a) == (category <$> b) = hylo construct runCofree <$> zipTerms a b
          | comparable a b = runAlgorithm construct recur cost (Just <$> algorithmWithTerms construct a b)
          | otherwise = Nothing

-- | Construct an algorithm to diff a pair of terms.
algorithmWithTerms :: (TermF (Syntax leaf) (Both a) diff -> diff) -> Term (Syntax leaf) a -> Term (Syntax leaf) a -> Algorithm (Term (Syntax leaf) a) diff diff
algorithmWithTerms construct t1 t2 = case (unwrap t1, unwrap t2) of
  (Indexed a, Indexed b) -> annotate . Indexed <$> bySimilarity a b
  (S.FunctionCall identifierA argsA, S.FunctionCall identifierB argsB) -> (annotate .) .
    S.FunctionCall <$> recursively identifierA identifierB
                   <*> bySimilarity argsA argsB
  (S.Switch exprA casesA, S.Switch exprB casesB) -> (annotate .) .
    S.Switch <$> recursively exprA exprB
             <*> bySimilarity casesA casesB
  (S.Object a, S.Object b) -> annotate . S.Object <$> bySimilarity a b
  (Commented commentsA a, Commented commentsB b) -> (annotate .) .
    Commented <$> bySimilarity commentsA commentsB
              <*> sequenceA (recursively <$> a <*> b)
  (Array a, Array b) -> annotate . Array <$> bySimilarity a b
  (S.Class identifierA paramsA expressionsA, S.Class identifierB paramsB expressionsB) -> ((annotate .) .) .
    S.Class <$> recursively identifierA identifierB
            <*> sequenceA (recursively <$> paramsA <*> paramsB)
            <*> bySimilarity expressionsA expressionsB
  (S.Method identifierA paramsA expressionsA, S.Method identifierB paramsB expressionsB) -> ((annotate .) .) .
    S.Method <$> recursively identifierA identifierB
             <*> bySimilarity paramsA paramsB
             <*> bySimilarity expressionsA expressionsB
  _ -> recursively t1 t2
  where annotate = construct . (both (extract t1) (extract t2) :<)

-- | Run an algorithm, given functions characterizing the evaluation.
runAlgorithm :: (GAlign f, Traversable f, HasField fields (Vector.Vector Double))
  => (CofreeF f (Both (Record fields)) (Free (CofreeF f (Both (Record fields))) (Patch (Cofree f (Record fields)))) -> Free (CofreeF f (Both (Record fields))) (Patch (Cofree f (Record fields)))) -- ^ A function to wrap up & possibly annotate every produced diff.
  -> (Cofree f (Record fields) -> Cofree f (Record fields) -> Maybe (Free (CofreeF f (Both (Record fields))) (Patch (Cofree f (Record fields))))) -- ^ A function to diff two subterms recursively, if they are comparable, or else return 'Nothing'.
  -> SES.Cost (Free (CofreeF f (Both (Record fields))) (Patch (Cofree f (Record fields)))) -- ^ A function to compute the cost of a given diff node.
  -> Algorithm (Cofree f (Record fields)) (Free (CofreeF f (Both (Record fields))) (Patch (Cofree f (Record fields)))) a -- ^ The algorithm to run.
  -> a
runAlgorithm construct recur cost = Algorithm.iter $ \case
  Recursive a b f -> f (maybe (replacing a b) (construct . (both (extract a) (extract b) :<)) $ do
    aligned <- galign (unwrap a) (unwrap b)
    traverse (these (Just . deleting) (Just . inserting) recur) aligned)
  ByIndex as bs f -> f (ses recur cost as bs)
  BySimilarity as bs f -> f (rws recur as bs)
