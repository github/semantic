{-# LANGUAGE DeriveAnyClass #-}
module Syntax where

import Data.Aeson
import Data.Align.Generic
import Data.Functor.Classes
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Listable
import Data.Mergeable
import Data.Text (pack)
import GHC.Generics
import Prologue

-- | A node in an abstract syntax tree.
--
-- 'f' is the type representing another level of the tree, e.g. the children of branches. Often 'Cofree', 'Free' or similar.
data Syntax f
  -- | A terminal syntax node, e.g. an identifier, or atomic literal.
  = Leaf Text
  -- | An ordered branch of child nodes, expected to be variadic in the grammar, e.g. a list of statements or uncurried function parameters.
  | Indexed [f]
  -- | An ordered branch of child nodes, expected to be of fixed length in the grammar, e.g. a binary operator & its operands.
  | Fixed [f]
  -- | A function call has an identifier where f is a (Leaf a) and a list of arguments.
  | FunctionCall f [f] [f]
  -- | A ternary has a condition, a true case and a false case
  | Ternary f [f]
  -- | An anonymous function has a list of expressions and params.
  | AnonymousFunction [f] [f]
  -- | A function has an identifier, possible type arguments, params, a possible type, and list of expressions.
  | Function f [f] [f]
  -- | An assignment has an identifier where f can be a member access, and the value is another syntax element (function call, leaf, etc.)
  | Assignment f f
  -- | An operator assignment represents expressions with operators like math (e.g x += 1) or conditional (e.g. x ||= 1) assignment.
  | OperatorAssignment f f
  -- | A member access contains a syntax, and another syntax that identifies a property or value in the first syntax.
  -- | e.g. in Javascript x.y represents a member access syntax.
  | MemberAccess f f
  -- | A method call consisting of its target, the method name, and the parameters passed to the method.
  -- | e.g. in Javascript console.log('hello') represents a method call.
  | MethodCall f f [f] [f]
  -- | An operator can be applied to a list of syntaxes.
  | Operator [f]
  -- | A variable declaration. e.g. var foo;
  | VarDecl [f]
  -- | A variable assignment in a variable declaration. var foo = bar;
  | VarAssignment [f] f
  -- | A subscript access contains a syntax, and another syntax that indefies a property or value in the first syntax.
  -- | e.g. in Javascript x["y"] represents a subscript access syntax.
  | SubscriptAccess f f
  | Switch [f] [f]
  | Case f [f]
  -- | A default case in a switch statement.
  | DefaultCase [f]
  | Select [f]
  | Object (Maybe f) [f]
  -- | A pair in an Object. e.g. foo: bar or foo => bar
  | Pair f f
  -- | A comment.
  | Comment Text
  -- | A term preceded or followed by any number of comments.
  | Commented [f] (Maybe f)
  | ParseError [f]
  -- | A for statement has a list of expressions to setup the iteration and then a list of expressions in the body.
  | For [f] [f]
  | DoWhile f f
  | While f [f]
  | Return [f]
  | Throw f
  | Constructor f
  -- | TODO: Is it a problem that in Ruby, this pattern can work for method def too?
  | Try [f] [f] (Maybe f) (Maybe f)
  -- | An array literal with list of children.
  | Array (Maybe f) [f]
  -- | A class with an identifier, superclass, and a list of definitions.
  | Class f [f] [f]
  -- | A method definition with an identifier, optional receiver, optional type arguments, params, optional return type, and a list of expressions.
  | Method [f] f (Maybe f) [f] [f]
  -- | An if statement with an expression and maybe more expression clauses.
  | If f [f]
  -- | A module with an identifier, and a list of syntaxes.
  | Module f [f]
  -- | An interface with an identifier, a list of clauses, and a list of declarations..
  | Interface f [f] [f]
  | Namespace f [f]
  | Import f [f]
  | Export (Maybe f) [f]
  | Yield [f]
  -- | A negation of a single expression.
  | Negate f
  -- | A rescue block has a list of arguments to rescue and a list of expressions.
  | Rescue [f] [f]
  | Go f
  | Defer f
  | TypeAssertion f f
  | TypeConversion f f
  -- | A struct with an optional type.
  | Struct (Maybe f) [f]
  | Break (Maybe f)
  | Continue (Maybe f)
  -- | A block statement has an ordered branch of child nodes, e.g. BEGIN {...} or END {...} in Ruby/Perl.
  | BlockStatement [f]
  -- | A parameter declaration with an optional type.
  | ParameterDecl (Maybe f) f
  -- | A type declaration has an identifier and a type.
  | TypeDecl f f
  -- | A field declaration with an optional type, and an optional tag.
  | FieldDecl [f]
  -- | A type.
  | Ty [f]
  -- | A send statement has a channel and an expression in Go.
  | Send f f
  deriving (Eq, Foldable, Functor, GAlign, Generic, Generic1, Mergeable, Ord, Show, Traversable, ToJSON, NFData)


extractLeafValue :: Syntax b -> Maybe Text
extractLeafValue syntax = case syntax of
  Leaf a -> Just a
  _ -> Nothing

-- Instances

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
    \/ liftCons2 (liftTiers recur) (liftTiers recur) Syntax.Object
    \/ liftCons2 recur recur Pair
    \/ liftCons1 (pack `mapT` tiers) Comment
    \/ liftCons2 (liftTiers recur) (liftTiers recur) Commented
    \/ liftCons1 (liftTiers recur) Syntax.ParseError
    \/ liftCons2 (liftTiers recur) (liftTiers recur) For
    \/ liftCons2 recur recur DoWhile
    \/ liftCons2 recur (liftTiers recur) While
    \/ liftCons1 (liftTiers recur) Return
    \/ liftCons1 recur Throw
    \/ liftCons1 recur Constructor
    \/ liftCons4 (liftTiers recur) (liftTiers recur) (liftTiers recur) (liftTiers recur) Try
    \/ liftCons2 (liftTiers recur) (liftTiers recur) Syntax.Array
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

instance Eq1 Syntax where
  liftEq = genericLiftEq
