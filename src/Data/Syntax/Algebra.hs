{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MonoLocalBinds, TypeOperators #-}
module Data.Syntax.Algebra
( FAlgebra
, RAlgebra
, fToR
, decoratorWithAlgebra
, identifierAlgebra
, cyclomaticComplexityAlgebra
) where

import Control.Comonad (extract)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.Functor.Foldable
import Data.Record
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Statement as Statement
import Data.Union
import Term

-- | An F-algebra on some carrier functor 'f'.
type FAlgebra f a = f a -> a

-- | An R-algebra on some carrier functor 'f' of its fixpoint type 't'.
type RAlgebra f t a = f (t, a) -> a

-- | Promote an FAlgebra into an RAlgebra (by dropping the original parameter).
fToR :: Functor (Base t) => FAlgebra (Base t) a -> RAlgebra (Base t) t a
fToR f = f . fmap snd

-- | Lift an algebra into a decorator for terms annotated with records.
decoratorWithAlgebra :: Functor f
                     => RAlgebra (Base (Term f (Record fs))) (Term f (Record fs)) a -- ^ An R-algebra on terms.
                     -> Term f (Record fs) -- ^ A term to decorate with values produced by the R-algebra.
                     -> Term f (Record (a ': fs)) -- ^ A term decorated with values produced by the R-algebra.
decoratorWithAlgebra alg = para $ \ c@(a :< f) -> cofree $ (alg (fmap (second (rhead . extract)) c) :. a) :< fmap snd f


newtype Identifier = Identifier ByteString
  deriving (Eq, Show)

-- | Produce the identifier for a given term, if any.
--
--   Identifier syntax is labelled, as well as declaration syntax identified by these, but other uses of these identifiers are not, e.g. the declaration of a class or method or binding of a variable will be labelled, but a function call will not.
identifierAlgebra :: (Syntax.Identifier :< fs, Declaration.Method :< fs, Declaration.Class :< fs, Traversable (Union fs)) => FAlgebra (Base (Term (Union fs) a)) (Maybe Identifier)
identifierAlgebra (_ :< union) = case union of
  _ | Just (Syntax.Identifier s) <- prj union -> Just (Identifier s)
  _ | Just Declaration.Class{..} <- prj union -> classIdentifier
  _ | Just Declaration.Method{..} <- prj union -> methodName
  _ -> Nothing

-- | The cyclomatic complexity of a (sub)term.
newtype CyclomaticComplexity = CyclomaticComplexity Int
  deriving (Enum, Eq, Num, Ord, Show)

-- | Compute the cyclomatic complexity of a (sub)term, measured as the number places where control exits scope, e.g. returns and yields.
--
--   TODO: Explicit returns at the end of methods should only count once.
--   TODO: Anonymous functions should not increase parent scope’s complexity.
--   TODO: Inner functions should not increase parent scope’s complexity.
cyclomaticComplexityAlgebra :: (Declaration.Method :< fs, Statement.Return :< fs, Statement.Yield :< fs, Traversable (Union fs)) => FAlgebra (Base (Term (Union fs) a)) CyclomaticComplexity
cyclomaticComplexityAlgebra (_ :< union) = case union of
  _ | Just Declaration.Method{} <- prj union -> succ (sum union)
  _ | Just Statement.Return{} <- prj union -> succ (sum union)
  _ | Just Statement.Yield{} <- prj union -> succ (sum union)
  _ -> sum union
