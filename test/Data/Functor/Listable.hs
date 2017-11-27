{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Functor.Listable
( Listable(..)
, mapT
, cons0
, cons1
, cons2
, cons3
, cons4
, cons5
, cons6
, (\/)
, Tier
, Listable1(..)
, tiers1
, Listable2(..)
, tiers2
, liftCons1
, liftCons2
, liftCons3
, liftCons4
, liftCons5
, ListableF(..)
, addWeight
, ofWeight
, ListableSyntax
) where

import qualified Category
import Control.Monad.Free as Free
import Control.Monad.Trans.Free as FreeF
import Data.ByteString (ByteString)
import Data.Char (chr)
import Data.Diff
import Data.Functor.Both
import Data.List.NonEmpty
import Data.Patch
import Data.Range
import Data.Record
import Data.Semigroup
import Data.Source
import Data.Span
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Statement as Statement
import Data.Term
import Data.Text as T (Text, pack)
import qualified Data.Text.Encoding as T
import Data.These
import Data.Union
import Diffing.Algorithm.RWS
import Renderer.TOC
import Syntax as S
import Test.LeanCheck
import qualified Language

type Tier a = [a]

-- | Lifting of 'Listable' to @* -> *@.
class Listable1 l where
  -- | The tiers for @l :: * -> *@, parameterized by the tiers for @a :: *@.
  liftTiers :: [Tier a] -> [Tier (l a)]

-- | A suitable definition of 'tiers' for 'Listable1' type constructors parameterized by 'Listable' types.
tiers1 :: (Listable a, Listable1 l) => [Tier (l a)]
tiers1 = liftTiers tiers


-- | Lifting of 'Listable' to @* -> * -> *@.
class Listable2 l where
  -- | The tiers for @l :: * -> * -> *@, parameterized by the tiers for @a :: *@ & @b :: *@.
  liftTiers2 :: [Tier a] -> [Tier b] -> [Tier (l a b)]

-- | A suitable definition of 'tiers' for 'Listable2' type constructors parameterized by 'Listable' types.
tiers2 :: (Listable a, Listable b, Listable2 l) => [Tier (l a b)]
tiers2 = liftTiers2 tiers tiers


class Listable3 l where
  liftTiers3 :: [Tier a] -> [Tier b] -> [Tier c] -> [Tier (l a b c)]

tiers3 :: (Listable3 l, Listable a, Listable b, Listable c) => [Tier (l a b c)]
tiers3 = liftTiers3 tiers tiers tiers


-- | Lifts a unary constructor to a list of tiers, given a list of tiers for its argument.
--
--   Commonly used in the definition of 'Listable1' and 'Listable2' instances.
liftCons1 :: [Tier a] -> (a -> b) -> [Tier b]
liftCons1 tiers f = mapT f tiers `addWeight` 1

-- | Lifts a binary constructor to a list of tiers, given lists of tiers for its arguments.
--
--   Commonly used in the definition of 'Listable1' and 'Listable2' instances.
liftCons2 :: [Tier a] -> [Tier b] -> (a -> b -> c) -> [Tier c]
liftCons2 tiers1 tiers2 f = mapT (uncurry f) (liftTiers2 tiers1 tiers2) `addWeight` 1

-- | Lifts a ternary constructor to a list of tiers, given lists of tiers for its arguments.
--
--   Commonly used in the definition of 'Listable1' and 'Listable2' instances.
liftCons3 :: [Tier a] -> [Tier b] -> [Tier c] -> (a -> b -> c -> d) -> [Tier d]
liftCons3 tiers1 tiers2 tiers3 f = mapT (uncurry3 f) (tiers1 >< tiers2 >< tiers3) `addWeight` 1
  where uncurry3 f (a, (b, c)) = f a b c

-- | Lifts a quaternary constructor to a list of tiers, given lists of tiers for its arguments.
--
--   Commonly used in the definition of 'Listable1' and 'Listable2' instances.
liftCons4 :: [Tier a] -> [Tier b] -> [Tier c] -> [Tier d] -> (a -> b -> c -> d -> e) -> [Tier e]
liftCons4 tiers1 tiers2 tiers3 tiers4 f = mapT (uncurry4 f) (tiers1 >< tiers2 >< tiers3 >< tiers4) `addWeight` 1
  where uncurry4 f (a, (b, (c, d))) = f a b c d

-- | Lifts a quinary constructor to a list of tiers, given lists of tiers for its arguments.
--
--   Commonly used in the definition of 'Listable1' and 'Listable2' instances.
liftCons5 :: [Tier a] -> [Tier b] -> [Tier c] -> [Tier d] -> [Tier e] -> (a -> b -> c -> d -> e -> f) -> [Tier f]
liftCons5 tiers1 tiers2 tiers3 tiers4 tiers5 f = mapT (uncurry5 f) (tiers1 >< tiers2 >< tiers3 >< tiers4 >< tiers5) `addWeight` 1
  where uncurry5 f (a, (b, (c, (d, e)))) = f a b c d e

-- | Convenient wrapper for 'Listable1' type constructors and 'Listable' types, where a 'Listable' instance would necessarily be orphaned.
newtype ListableF f a = ListableF { unListableF :: f a }
  deriving Show


-- Instances

instance Listable1 Maybe where
  liftTiers tiers = cons0 Nothing \/ liftCons1 tiers Just

instance Listable2 (,) where
  liftTiers2 = (><)

instance Listable2 Either where
  liftTiers2 leftTiers rightTiers = liftCons1 leftTiers Left \/ liftCons1 rightTiers Right

instance Listable a => Listable1 ((,) a) where
  liftTiers = liftTiers2 tiers

instance Listable1 [] where
  liftTiers tiers = go
    where go = cons0 [] \/ liftCons2 tiers go (:)

instance Listable1 NonEmpty where
  liftTiers tiers = liftCons2 tiers (liftTiers tiers) (:|)

instance Listable2 p => Listable1 (Join p) where
  liftTiers tiers = liftCons1 (liftTiers2 tiers tiers) Join

instance Listable2 These where
  liftTiers2 this that = liftCons1 this This \/ liftCons1 that That \/ liftCons2 this that These

instance Listable1 f => Listable2 (FreeF f) where
  liftTiers2 pureTiers recurTiers = liftCons1 pureTiers FreeF.Pure \/ liftCons1 (liftTiers recurTiers) FreeF.Free

instance (Listable1 f, Listable a) => Listable1 (FreeF f a) where
  liftTiers = liftTiers2 tiers

instance (Functor f, Listable1 f) => Listable1 (Free.Free f) where
  liftTiers pureTiers = go
    where go = liftCons1 (liftTiers2 pureTiers go) free
          free (FreeF.Free f) = Free.Free f
          free (FreeF.Pure a) = Free.Pure a

instance (Listable1 f, Listable a) => Listable (ListableF f a) where
  tiers = ListableF `mapT` tiers1


instance Listable1 f => Listable2 (TermF f) where
  liftTiers2 annotationTiers recurTiers = liftCons2 annotationTiers (liftTiers recurTiers) In

instance (Listable1 f, Listable a) => Listable1 (TermF f a) where
  liftTiers = liftTiers2 tiers

instance (Listable1 f, Listable a, Listable b) => Listable (TermF f a b) where
  tiers = tiers1

instance Listable1 f => Listable1 (Term f) where
  liftTiers annotationTiers = go
    where go = liftCons1 (liftTiers2 annotationTiers go) Term

instance (Listable1 f, Listable a) => Listable (Term f a) where
  tiers = tiers1


instance (Listable1 syntax) => Listable3 (DiffF syntax) where
  liftTiers3 ann1Tiers ann2Tiers recurTiers
    =  liftCons1 (liftTiers2 (liftTiers2 ann1Tiers recurTiers) (liftTiers2 ann2Tiers recurTiers)) Patch
    \/ liftCons1 (liftTiers2 (liftTiers2 ann1Tiers ann2Tiers) recurTiers) Merge

instance (Listable1 syntax, Listable ann1, Listable ann2, Listable recur) => Listable (DiffF syntax ann1 ann2 recur) where
  tiers = tiers3

instance Listable1 f => Listable2 (Diff f) where
  liftTiers2 annTiers1 annTiers2 = go where go = liftCons1 (liftTiers3 annTiers1 annTiers2 go) Diff

instance (Listable1 syntax, Listable ann1, Listable ann2) => Listable (Diff syntax ann1 ann2) where
  tiers = tiers2


instance (Listable head, Listable (Record tail)) => Listable (Record (head ': tail)) where
  tiers = cons2 (:.)

instance Listable (Record '[]) where
  tiers = cons0 Nil


instance Listable Category.Category where
  tiers = cons0 Category.Program
       \/ cons0 Category.ParseError
       \/ cons0 Category.Boolean
       \/ cons0 Category.BooleanOperator
       \/ cons0 Category.FunctionCall
       \/ cons0 Category.Function
       \/ cons0 Category.Identifier
       \/ cons0 Category.MethodCall
       \/ cons0 Category.StringLiteral
       \/ cons0 Category.IntegerLiteral
       \/ cons0 Category.NumberLiteral
       \/ cons0 Category.Return
       \/ cons0 Category.If
       \/ cons0 Category.Class
       \/ cons0 Category.Method
       \/ cons0 Category.Binary
       \/ cons0 Category.Unary
       \/ cons0 Category.SingletonMethod


instance Listable2 Patch where
  liftTiers2 t1 t2 = liftCons1 t2 Insert \/ liftCons1 t1 Delete \/ liftCons2 t1 t2 Replace

instance (Listable a, Listable b) => Listable (Patch a b) where
  tiers = tiers2


instance Listable1 Syntax where
  liftTiers recur
    =  liftCons1 (pack `mapT` tiers) Leaf
    \/ liftCons1 (liftTiers recur) Indexed
    \/ liftCons1 (liftTiers recur) Fixed
    \/ liftCons3 recur (liftTiers recur) (liftTiers recur) FunctionCall
    \/ liftCons2 recur (liftTiers recur) Ternary
    \/ liftCons2 (liftTiers recur) (liftTiers recur) AnonymousFunction
    \/ liftCons3 recur (liftTiers recur) (liftTiers recur) Function
    \/ liftCons2 recur recur Assignment
    \/ liftCons2 recur recur OperatorAssignment
    \/ liftCons2 recur recur MemberAccess
    \/ liftCons4 recur recur (liftTiers recur) (liftTiers recur) MethodCall
    \/ liftCons1 (liftTiers recur) Operator
    \/ liftCons1 (liftTiers recur) VarDecl
    \/ liftCons2 (liftTiers recur) recur VarAssignment
    \/ liftCons2 recur recur SubscriptAccess
    \/ liftCons2 (liftTiers recur) (liftTiers recur) Switch
    \/ liftCons2 recur (liftTiers recur) Case
    \/ liftCons1 (liftTiers recur) Select
    \/ liftCons2 (liftTiers recur) (liftTiers recur) S.Object
    \/ liftCons2 recur recur S.Pair
    \/ liftCons1 (pack `mapT` tiers) Comment
    \/ liftCons2 (liftTiers recur) (liftTiers recur) Commented
    \/ liftCons1 (liftTiers recur) S.ParseError
    \/ liftCons2 (liftTiers recur) (liftTiers recur) For
    \/ liftCons2 recur recur DoWhile
    \/ liftCons2 recur (liftTiers recur) While
    \/ liftCons1 (liftTiers recur) Return
    \/ liftCons1 recur Throw
    \/ liftCons1 recur Constructor
    \/ liftCons4 (liftTiers recur) (liftTiers recur) (liftTiers recur) (liftTiers recur) Try
    \/ liftCons2 (liftTiers recur) (liftTiers recur) S.Array
    \/ liftCons3 recur (liftTiers recur) (liftTiers recur) Class
    \/ liftCons5 (liftTiers recur) recur (liftTiers recur) (liftTiers recur) (liftTiers recur) Method
    \/ liftCons2 recur (liftTiers recur) If
    \/ liftCons2 recur (liftTiers recur) Module
    \/ liftCons2 recur (liftTiers recur) Namespace
    \/ liftCons2 recur (liftTiers recur) Import
    \/ liftCons2 (liftTiers recur) (liftTiers recur) Export
    \/ liftCons1 (liftTiers recur) Yield
    \/ liftCons1 recur Negate
    \/ liftCons2 (liftTiers recur) (liftTiers recur) Rescue
    \/ liftCons1 recur Go
    \/ liftCons1 recur Defer
    \/ liftCons2 recur recur TypeAssertion
    \/ liftCons2 recur recur TypeConversion
    \/ liftCons1 (liftTiers recur) Break
    \/ liftCons1 (liftTiers recur) Continue
    \/ liftCons1 (liftTiers recur) BlockStatement
    \/ liftCons2 (liftTiers recur) recur ParameterDecl
    \/ liftCons2 recur recur TypeDecl
    \/ liftCons1 (liftTiers recur) FieldDecl
    \/ liftCons1 (liftTiers recur) Ty
    \/ liftCons2 recur recur Send
    \/ liftCons1 (liftTiers recur) DefaultCase

instance Listable recur => Listable (Syntax recur) where
  tiers = tiers1


instance (Listable1 f, Listable1 (Union (g ': fs))) => Listable1 (Union (f ': g ': fs)) where
  liftTiers tiers = (inj `mapT` ((liftTiers :: [Tier a] -> [Tier (f a)]) tiers)) \/ (weaken `mapT` ((liftTiers :: [Tier a] -> [Tier (Union (g ': fs) a)]) tiers))

instance Listable1 f => Listable1 (Union '[f]) where
  liftTiers tiers = inj `mapT` ((liftTiers :: [Tier a] -> [Tier (f a)]) tiers)

instance (Listable1 (Union fs), Listable a) => Listable (Union fs a) where
  tiers = tiers1


instance Listable1 Comment.Comment where
  liftTiers _ = cons1 Comment.Comment

instance Listable1 Declaration.Function where
  liftTiers tiers = liftCons4 (liftTiers tiers) tiers (liftTiers tiers) tiers Declaration.Function

instance Listable1 Declaration.Method where
  liftTiers tiers = liftCons5 (liftTiers tiers) tiers tiers (liftTiers tiers) tiers Declaration.Method

instance Listable1 Statement.If where
  liftTiers tiers = liftCons3 tiers tiers tiers Statement.If

instance Listable1 Statement.Return where
  liftTiers tiers = liftCons1 tiers Statement.Return

instance Listable1 Syntax.Context where
  liftTiers tiers = liftCons2 (liftTiers tiers) tiers Syntax.Context

instance Listable1 Syntax.Empty where
  liftTiers _ = cons0 Syntax.Empty

instance Listable1 Syntax.Identifier where
  liftTiers _ = cons1 Syntax.Identifier

type ListableSyntax = Union
  '[ Comment.Comment
   , Declaration.Function
   , Declaration.Method
   , Statement.If
   , Syntax.Context
   , Syntax.Empty
   , Syntax.Identifier
   , []
   ]


instance Listable1 Gram where
  liftTiers tiers = liftCons2 (liftTiers (liftTiers tiers)) (liftTiers (liftTiers tiers)) Gram

instance Listable a => Listable (Gram a) where
  tiers = tiers1


instance Listable Text where
  tiers = pack `mapT` tiers

instance Listable Declaration where
  tiers
    =  cons4 MethodDeclaration
    \/ cons3 FunctionDeclaration
    \/ cons2 (\ a b -> ErrorDeclaration a b Nothing)

instance Listable Language.Language where
  tiers
    =  cons0 Language.Go
    \/ cons0 Language.JavaScript
    \/ cons0 Language.Python
    \/ cons0 Language.Ruby
    \/ cons0 Language.TypeScript

instance Listable Range where
  tiers = cons2 Range


instance Listable Pos where
  tiers = cons2 Pos

instance Listable Span where
  tiers = cons2 Span


instance Listable Source where
  tiers = fromBytes `mapT` tiers

instance Listable ByteString where
  tiers = (T.encodeUtf8 . T.pack) `mapT` strings
    where strings = foldr ((\\//) . listsOf . toTiers) []
            [ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']
            , [' '..'/'] <> [':'..'@'] <> ['['..'`'] <> ['{'..'~']
            , [chr 0x00..chr 0x1f] <> [chr 127] -- Control characters.
            , [chr 0xa0..chr 0x24f] ] -- Non-ASCII.
