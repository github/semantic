{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, TypeApplications, TypeFamilies #-}
module Parsing.Parser
( Parser(..)
  -- * Parsers
  -- $abstract
, SomeParser(..)
, goParser
, javaParser
, javascriptParser
, jsonParser
, jsxParser
, markdownParser
, phpParser
, pythonParserALaCarte
, pythonParserPrecise
, pythonParser
, rubyParser
, tsxParser
, typescriptParser
  -- * Modes by term type
, TermMode
  -- * Canonical sets of parsers
, aLaCarteParsers
, preciseParsers
, allParsers
) where

import           Assigning.Assignment
import qualified CMarkGFM
import           Data.AST
import           Data.Language
import qualified Data.Map as Map
import qualified Data.Syntax as Syntax
import           Data.Term
import           Foreign.Ptr
import qualified Language.Go.Assignment as Go
import qualified Language.Java as Java
import qualified Language.JSON as JSON
import qualified Language.Markdown.Assignment as Markdown
import qualified Language.PHP.Assignment as PHP
import qualified Language.Python as PythonPrecise
import qualified Language.Python.Assignment as PythonALaCarte
import qualified Language.Ruby.Assignment as Ruby
import qualified Language.TSX.Assignment as TSX
import qualified Language.TypeScript.Assignment as TypeScript
import           Prelude hiding (fail)
import           Prologue
import           TreeSitter.Go
import qualified TreeSitter.Language as TS (Language, Symbol)
import           TreeSitter.PHP
import           TreeSitter.Python
import           TreeSitter.Ruby (tree_sitter_ruby)
import           TreeSitter.TSX
import           TreeSitter.TypeScript
import           TreeSitter.Unmarshal

-- | A parser from 'Source' onto some term type.
data Parser term where
  -- | A parser producing 'AST' using a 'TS.Language'.
  ASTParser :: (Bounded grammar, Enum grammar, Show grammar) => Ptr TS.Language -> Parser (AST [] grammar)
  -- | A parser 'Unmarshal'ing to a precise AST type using a 'TS.Language'.
  UnmarshalParser :: Unmarshal t => Ptr TS.Language -> Parser (t Loc)
  -- | A parser producing an à la carte term given an 'AST'-producing parser and an 'Assignment' onto 'Term's in some syntax type.
  AssignmentParser :: (TS.Symbol grammar, Syntax.HasErrors term, Eq1 ast, Foldable term, Foldable ast, Functor ast)
                   => Parser (AST ast grammar)          -- ^ A parser producing AST.
                   -> Assignment ast grammar (term Loc) -- ^ An assignment from AST onto 'Term's.
                   -> Parser (term Loc)                 -- ^ A parser producing 'Term's.
  -- | A parser for 'Markdown' using cmark.
  MarkdownParser :: Parser (AST (TermF [] CMarkGFM.NodeType) Markdown.Grammar)


-- $abstract
-- Most of our features are intended to operate over multiple languages, each represented by disjoint term types. Thus, we typically implement them using typeclasses, allowing us to share a single interface to invoke the feature, while specializing the implementation(s) as appropriate for each distinct term type.
--
-- In order to accomplish this, we employ 'SomeParser', which abstracts over parsers of various term types, while ensuring that some desired constraint holds. Constructing a @'SomeParser' c@ requires satisfiyng the constraint @c@ against the underlying 'Parser'’s term type, and so it can be used to parse with any of a map of parsers whose terms support @c@.
--
-- In practice, this means using 'Control.Effect.Parse.parseWith', and passing in a map of parsers to select from for your feature. It is recommended to define the map as a concrete top-level binding using the abstract parsers or ideally the canonical maps of parsers, below; using the abstracted parsers or canonical maps directly with 'Control.Effect.Parse.parseWith' will lead to significantly slower compiles.
--
-- Bad:
--
-- @
-- isFancy :: (Carrier sig m, Member Parse sig) => Blob -> m Bool
-- isFancy = parseWith (preciseParsers @Fancy) (pure . isTermFancy) -- slow compiles!
-- @
--
-- Good:
--
-- @
-- fancyParsers :: 'Map' 'Language' ('SomeParser' Fancy 'Loc')
-- fancyParsers = preciseParsers
--
-- isFancy :: (Carrier sig m, Member Parse sig) => Blob -> m Bool
-- isFancy = parseWith fancyParsers (pure . isTermFancy) -- much faster compiles
-- @


-- | A parser producing terms of existentially-quantified type under some constraint @c@.
--
--   This can be used to perform actions on terms supporting some feature abstracted using a typeclass, without knowing (or caring) what the specific term types are.
data SomeParser c a where
  SomeParser :: c t => Parser (t a) -> SomeParser c a

goParser :: c Go.Term => (Language, SomeParser c Loc)
goParser = (Go, SomeParser (AssignmentParser (ASTParser tree_sitter_go) Go.assignment))

javaParser :: c Java.Term => (Language, SomeParser c Loc)
javaParser = (Java, SomeParser (UnmarshalParser @Java.Term Java.tree_sitter_java))

javascriptParser :: c TSX.Term => (Language, SomeParser c Loc)
javascriptParser = (JavaScript, SomeParser (AssignmentParser (ASTParser tree_sitter_tsx) TSX.assignment))

jsonParser :: c JSON.Term => (Language, SomeParser c Loc)
jsonParser = (JSON, SomeParser (UnmarshalParser @JSON.Term JSON.tree_sitter_json))

jsxParser :: c TSX.Term => (Language, SomeParser c Loc)
jsxParser = (JSX, SomeParser (AssignmentParser (ASTParser tree_sitter_tsx) TSX.assignment))

markdownParser :: c Markdown.Term => (Language, SomeParser c Loc)
markdownParser = (Markdown, SomeParser (AssignmentParser MarkdownParser Markdown.assignment))

phpParser :: c PHP.Term => (Language, SomeParser c Loc)
phpParser = (PHP, SomeParser (AssignmentParser (ASTParser tree_sitter_php) PHP.assignment))

pythonParserALaCarte :: c PythonALaCarte.Term => (Language, SomeParser c Loc)
pythonParserALaCarte = (Python, SomeParser (AssignmentParser (ASTParser tree_sitter_python) PythonALaCarte.assignment))

pythonParserPrecise :: c PythonPrecise.Term => (Language, SomeParser c Loc)
pythonParserPrecise = (Python, SomeParser (UnmarshalParser @PythonPrecise.Term PythonPrecise.tree_sitter_python))

pythonParser :: (c PythonALaCarte.Term, c PythonPrecise.Term) => PerLanguageModes -> (Language, SomeParser c Loc)
pythonParser modes = case pythonMode modes of
  ALaCarte -> pythonParserALaCarte
  Precise  -> pythonParserPrecise

rubyParser :: c Ruby.Term => (Language, SomeParser c Loc)
rubyParser = (Ruby, SomeParser (AssignmentParser (ASTParser tree_sitter_ruby) Ruby.assignment))

tsxParser :: c TSX.Term => (Language, SomeParser c Loc)
tsxParser = (TSX, SomeParser (AssignmentParser (ASTParser tree_sitter_tsx) TSX.assignment))

typescriptParser :: c TypeScript.Term => (Language, SomeParser c Loc)
typescriptParser = (TypeScript, SomeParser (AssignmentParser (ASTParser tree_sitter_typescript) TypeScript.assignment))


-- | A type family selecting the language mode for a given term type.
type family TermMode term where
  TermMode Java.Term          = 'Precise
  TermMode JSON.Term          = 'Precise
  TermMode PythonPrecise.Term = 'Precise
  TermMode _                  = 'ALaCarte


-- | The canonical set of parsers producing à la carte terms.
aLaCarteParsers
  :: ( c Go.Term
     , c Markdown.Term
     , c PHP.Term
     , c PythonALaCarte.Term
     , c Ruby.Term
     , c TSX.Term
     , c TypeScript.Term
     )
  => Map Language (SomeParser c Loc)
aLaCarteParsers = Map.fromList
  [ goParser
  , javascriptParser
  , jsxParser
  , markdownParser
  , phpParser
  , pythonParserALaCarte
  , rubyParser
  , typescriptParser
  , tsxParser
  ]

-- | The canonical set of parsers producing precise terms.
preciseParsers
  :: ( c Java.Term
     , c JSON.Term
     , c PythonPrecise.Term
     )
  => Map Language (SomeParser c Loc)
preciseParsers = Map.fromList
  [ javaParser
  , jsonParser
  , pythonParserPrecise
  ]

-- | The canonical set of all parsers for the passed per-language modes.
allParsers
  :: ( c Go.Term
     , c Java.Term
     , c JSON.Term
     , c Markdown.Term
     , c PHP.Term
     , c PythonALaCarte.Term
     , c PythonPrecise.Term
     , c Ruby.Term
     , c TSX.Term
     , c TypeScript.Term
     )
  => PerLanguageModes
  -> Map Language (SomeParser c Loc)
allParsers modes = Map.fromList
  [ goParser
  , javaParser
  , javascriptParser
  , jsonParser
  , jsxParser
  , markdownParser
  , phpParser
  , pythonParser modes
  , rubyParser
  , typescriptParser
  , tsxParser
  ]
