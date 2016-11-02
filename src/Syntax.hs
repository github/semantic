{-# LANGUAGE DeriveAnyClass #-}
module Syntax where

import Prologue
import Data.Mergeable
import GHC.Generics
import Test.QuickCheck hiding (Fixed)

-- | A node in an abstract syntax tree.
--
-- 'a' is the type of leaves in the syntax tree, typically 'Text', but possibly some datatype representing different leaves more precisely.
-- 'f' is the type representing another level of the tree, e.g. the children of branches. Often 'Cofree', 'Free' or similar.
data Syntax a f
  -- | A terminal syntax node, e.g. an identifier, or atomic literal.
  = Leaf a
  -- | An ordered branch of child nodes, expected to be variadic in the grammar, e.g. a list of statements or uncurried function parameters.
  | Indexed [f]
  -- | An ordered branch of child nodes, expected to be of fixed length in the grammar, e.g. a binary operator & its operands.
  | Fixed [f]
  -- | A function call has an identifier where f is a (Leaf a) and a list of arguments.
  | FunctionCall f [f]
  -- | A ternary has a condition, a true case and a false case
  | Ternary { ternaryCondition :: f, ternaryCases :: [f] }
  -- | An anonymous function has a list of expressions and params.
  | AnonymousFunction { params :: [f], expressions :: [f] }
  -- | A function has a list of expressions.
  | Function { id :: f, params :: [f], expressions :: [f] }
  -- | An assignment has an identifier where f can be a member access, and the value is another syntax element (function call, leaf, etc.)
  | Assignment { assignmentId :: f, value :: f }
  -- | A math assignment represents expressions whose operator classifies as mathy (e.g. += or *=).
  | MathAssignment { mathAssignmentId :: f, value :: f }
  -- | A member access contains a syntax, and another syntax that identifies a property or value in the first syntax.
  -- | e.g. in Javascript x.y represents a member access syntax.
  | MemberAccess { memberId :: f, property :: f }
  -- | A method call consisting of its target, the method name, and the parameters passed to the method.
  -- | e.g. in Javascript console.log('hello') represents a method call.
  | MethodCall { targetId :: f, methodId :: f, methodParams :: [f] }
  -- | The list of arguments to a method call.
  -- | TODO: It might be worth removing this and using Fixed instead.
  | Args [f]
  -- | An operator can be applied to a list of syntaxes.
  | Operator [f]
  -- | A variable declaration. e.g. var foo;
  | VarDecl f
  -- | A variable assignment in a variable declaration. var foo = bar;
  | VarAssignment { varId :: f, varValue :: f }
  -- | A subscript access contains a syntax, and another syntax that indefies a property or value in the first syntax.
  -- | e.g. in Javascript x["y"] represents a subscript access syntax.
  | SubscriptAccess { subscriptId :: f, subscriptElement :: f }
  | Switch { switchExpr :: f, cases :: [f] }
  | Case { caseExpr :: f, caseStatements :: f }
  | Object { keyValues :: [f] }
  -- | A pair in an Object. e.g. foo: bar or foo => bar
  | Pair f f
  -- | A comment.
  | Comment a
  -- | A term preceded or followed by any number of comments.
  | Commented [f] (Maybe f)
  | Error [f]
  -- | A for statement has a list of expressions to setup the iteration and then a list of expressions in the body.
  | For [f] [f]
  | DoWhile { doWhileBody :: f, doWhileExpr :: f }
  | While { whileExpr :: f, whileBody :: [f] }
  | Return (Maybe f)
  | Throw f
  | Constructor f
  | Try f (Maybe f) (Maybe f)
  -- | An array literal with list of children.
  | Array [f]
  -- | A class with an identifier, superclass, and a list of definitions.
  | Class f (Maybe f) [f]
  -- | A method definition with an identifier, params, and a list of expressions.
  | Method f [f] [f]
  -- | An if statement with an expression and maybe more expression clauses.
  | If f [f]
  -- | A module with an identifier, and a list of syntaxes.
  | Module { moduleId:: f, moduleBody :: [f] }
  | Import f [f]
  | Export (Maybe f) [f]
  -- | A conditional assignment represents expressions whose operator classifies as conditional (e.g. ||= or &&=).
  | ConditionalAssignment { conditionalAssignmentId :: f, value :: f }
  | Yield (Maybe f)
  | Until { untilExpr :: f, untilBody :: [f] }
  -- | An unless statement with an expression and maybe more expression clauses.
  | Unless f [f]
  | Begin [f]
  | Else [f]
  deriving (Eq, Foldable, Functor, Generic, Generic1, Mergeable, Ord, Show, Traversable)


-- Instances

instance (Arbitrary leaf, Arbitrary f) => Arbitrary (Syntax leaf f) where
  arbitrary = sized (syntaxOfSize (`resize` arbitrary) )

  shrink = genericShrink

syntaxOfSize :: Arbitrary leaf => (Int -> Gen f) -> Int -> Gen (Syntax leaf f)
syntaxOfSize recur n | n <= 1 = oneof $ (Leaf <$> arbitrary) : branchGeneratorsOfSize n
                     | otherwise = oneof $ branchGeneratorsOfSize n
  where branchGeneratorsOfSize n =
          [ Indexed <$> childrenOfSize (pred n)
          , Fixed <$> childrenOfSize (pred n)
          ]
        childrenOfSize n | n <= 0 = pure []
        childrenOfSize n = do
          m <- choose (1, n)
          first <- recur m
          rest <- childrenOfSize (n - m)
          pure $! first : rest
