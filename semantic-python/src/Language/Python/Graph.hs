module Language.Python.Graph (graph) where

import           Algebra.Graph.NonEmpty (Graph)
import qualified Algebra.Graph.NonEmpty as Graph
import qualified TreeSitter.Python.AST as Py

type Text = String

data Node a = Link Link a
            | Search Search a
            | Scope Scope a

data Link
  = Root
  | Internal
  | Exported
  | Top
  | Curr

data Search
  = Search
  | False Text
  | Pop Text

data Scope = Push
           | Ignore
           | Jump

mod1 :: Graph Node
mod1 = E
