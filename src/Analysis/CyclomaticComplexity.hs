{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators #-}
module Analysis.CyclomaticComplexity
( cyclomaticComplexityAlgebra
) where

import Data.Algebra (FAlgebra)
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Statement as Statement
import Data.Term
import Data.Union

-- | The cyclomatic complexity of a (sub)term.
newtype CyclomaticComplexity = CyclomaticComplexity Int
  deriving (Enum, Eq, Num, Ord, Show)

-- | Compute the cyclomatic complexity of a (sub)term, measured as the number places where control exits scope, e.g. returns and yields.
--
--   TODO: Explicit returns at the end of methods should only count once.
--   TODO: Anonymous functions should not increase parent scope’s complexity.
--   TODO: Inner functions should not increase parent scope’s complexity.
cyclomaticComplexityAlgebra :: (Declaration.Method :< fs, Statement.Return :< fs, Statement.Yield :< fs, Apply Foldable fs, Apply Functor fs) => FAlgebra (Term (Union fs) a) CyclomaticComplexity
cyclomaticComplexityAlgebra (In _ union) = case union of
  _ | Just Declaration.Method{} <- prj union -> succ (sum union)
  _ | Just Statement.Return{} <- prj union -> succ (sum union)
  _ | Just Statement.Yield{} <- prj union -> succ (sum union)
  _ -> sum union
