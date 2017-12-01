{-# LANGUAGE DeriveAnyClass #-}
module Syntax where

import Diffing.Algorithm
import Data.Aeson (ToJSON, (.=))
import Data.Align.Generic
import Data.Foldable (toList)
import Data.Functor.Classes
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import Data.JSON.Fields
import Data.Mergeable
import Data.Text (Text)
import GHC.Generics

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
  | AnonymousFunction  [f] [f]
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
  deriving (Eq, Foldable, Functor, GAlign, Generic, Generic1, Mergeable, Ord, Show, Traversable, ToJSON)


extractLeafValue :: Syntax a -> Maybe Text
extractLeafValue syntax = case syntax of
  Leaf a -> Just a
  _ -> Nothing

-- Instances

instance Eq1 Syntax where liftEq = genericLiftEq
instance Show1 Syntax where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Syntax where
  toJSONFields1 syntax = [ "children" .= toList syntax ]

instance Diffable Syntax where
  algorithmFor s1 s2 = case (s1, s2) of
    (Indexed a, Indexed b) ->
      Indexed <$> byRWS a b
    (Module idA a, Module idB b) ->
      Module <$> diff idA idB <*> byRWS a b
    (FunctionCall identifierA typeParamsA argsA, FunctionCall identifierB typeParamsB argsB) ->
      FunctionCall <$> diff identifierA identifierB
                   <*> byRWS typeParamsA typeParamsB
                   <*> byRWS argsA argsB
    (Switch exprA casesA, Switch exprB casesB) ->
      Switch <$> byRWS exprA exprB
             <*> byRWS casesA casesB
    (Object tyA a, Object tyB b) ->
      Object <$> diffMaybe tyA tyB
             <*> byRWS a b
    (Commented commentsA a, Commented commentsB b) ->
      Commented <$> byRWS commentsA commentsB
                <*> diffMaybe a b
    (Array tyA a, Array tyB b) ->
      Array <$> diffMaybe tyA tyB
            <*> byRWS a b
    (Class identifierA clausesA expressionsA, Class identifierB clausesB expressionsB) ->
      Class <$> diff identifierA identifierB
            <*> byRWS clausesA clausesB
            <*> byRWS expressionsA expressionsB
    (Method clausesA identifierA receiverA paramsA expressionsA, Method clausesB identifierB receiverB paramsB expressionsB) ->
      Method <$> byRWS clausesA clausesB
             <*> diff identifierA identifierB
             <*> diffMaybe receiverA receiverB
             <*> byRWS paramsA paramsB
             <*> byRWS expressionsA expressionsB
    (Function idA paramsA bodyA, Function idB paramsB bodyB) ->
      Function <$> diff idA idB
               <*> byRWS paramsA paramsB
               <*> byRWS bodyA bodyB
    _ -> galignWith diffThese s1 s2
