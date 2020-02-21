{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Parsing.Parser
( Parser(..)
  -- * Parsers
  -- $abstract
, SomeParser(..)
, goParser
, goParserALaCarte
, goParserPrecise
, javaParser
, javascriptParserALaCarte
, javascriptParserPrecise
, javascriptParser
, jsonParser
, jsxParserALaCarte
, jsxParserPrecise
, jsxParser
, phpParserPrecise
, pythonParserALaCarte
, pythonParserPrecise
, pythonParser
, rubyParserALaCarte
, rubyParserPrecise
, rubyParser
, tsxParserALaCarte
, tsxParserPrecise
, tsxParser
, typescriptParserALaCarte
, typescriptParserPrecise
, typescriptParser
  -- * Modes by term type
, TermMode
  -- * Canonical sets of parsers
, aLaCarteParsers
, preciseParsers
, allParsers
) where

import           Assigning.Assignment
import           AST.Unmarshal
import           Data.AST
import           Data.Language
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Syntax as Syntax
import           Foreign.Ptr
import qualified Language.Go as GoPrecise
import qualified Language.Go.Assignment as GoALaCarte
import           Language.Go.Grammar
import qualified Language.Java as Java
import qualified Language.JSON as JSON
import qualified Language.PHP as PHPPrecise
import qualified Language.Python as PythonPrecise
import qualified Language.Python.Assignment as PythonALaCarte
import           Language.Python.Grammar
import qualified Language.Ruby as RubyPrecise
import qualified Language.Ruby.Assignment as RubyALaCarte
import           Language.Ruby.Grammar (tree_sitter_ruby)
import qualified Language.TSX as TSXPrecise
import qualified Language.TSX.Assignment as TSXALaCarte
import qualified Language.TypeScript as TypeScriptPrecise
import qualified Language.TypeScript.Assignment as TypeScriptALaCarte
import           Language.TypeScript.Grammar
import           Prelude hiding (fail)
import qualified TreeSitter.Language as TS (Language, Symbol)
import           TreeSitter.TSX

-- | A parser from 'Source' onto some term type.
data Parser term where
  -- | A parser producing 'AST' using a 'TS.Language'.
  ASTParser :: (Bounded grammar, Enum grammar, Show grammar) => Ptr TS.Language -> Parser (AST grammar)
  -- | A parser 'Unmarshal'ing to a precise AST type using a 'TS.Language'.
  UnmarshalParser :: Unmarshal t => Ptr TS.Language -> Parser (t Loc)
  -- | A parser producing an à la carte term given an 'AST'-producing parser and an 'Assignment' onto 'Term's in some syntax type.
  AssignmentParser :: (TS.Symbol grammar, Syntax.HasErrors term, Foldable term)
                   => Parser (AST grammar)          -- ^ A parser producing AST.
                   -> Assignment grammar (term Loc) -- ^ An assignment from AST onto 'Term's.
                   -> Parser (term Loc)                 -- ^ A parser producing 'Term's.


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

goParserALaCarte :: c GoALaCarte.Term => (Language, SomeParser c Loc)
goParserALaCarte = (Go, SomeParser (AssignmentParser (ASTParser tree_sitter_go) GoALaCarte.assignment))

goParserPrecise :: c GoPrecise.Term => (Language, SomeParser c Loc)
goParserPrecise = (Go, SomeParser (UnmarshalParser @GoPrecise.Term GoPrecise.tree_sitter_go))

goParser :: (c GoALaCarte.Term, c GoPrecise.Term) => PerLanguageModes -> (Language, SomeParser c Loc)
goParser modes = case goMode modes of
  ALaCarte -> goParserALaCarte
  Precise  -> goParserPrecise

javaParser :: c Java.Term => (Language, SomeParser c Loc)
javaParser = (Java, SomeParser (UnmarshalParser @Java.Term Java.tree_sitter_java))

javascriptParserALaCarte :: c TSXALaCarte.Term => (Language, SomeParser c Loc)
javascriptParserALaCarte = (JavaScript, SomeParser (AssignmentParser (ASTParser tree_sitter_tsx) TSXALaCarte.assignment))

javascriptParserPrecise :: c TSXPrecise.Term => (Language, SomeParser c Loc)
javascriptParserPrecise = (JavaScript, SomeParser (UnmarshalParser @TSXPrecise.Term TSXPrecise.tree_sitter_tsx))

javascriptParser :: (c TSXALaCarte.Term, c TSXPrecise.Term) => PerLanguageModes -> (Language, SomeParser c Loc)
javascriptParser modes = case javascriptMode modes of
  ALaCarte -> javascriptParserALaCarte
  Precise  -> javascriptParserPrecise

jsonParser :: c JSON.Term => (Language, SomeParser c Loc)
jsonParser = (JSON, SomeParser (UnmarshalParser @JSON.Term JSON.tree_sitter_json))

jsxParserALaCarte :: c TSXALaCarte.Term => (Language, SomeParser c Loc)
jsxParserALaCarte = (JSX, SomeParser (AssignmentParser (ASTParser tree_sitter_tsx) TSXALaCarte.assignment))

jsxParserPrecise :: c TSXPrecise.Term => (Language, SomeParser c Loc)
jsxParserPrecise = (JSX, SomeParser (UnmarshalParser @TSXPrecise.Term TSXPrecise.tree_sitter_tsx))

jsxParser :: (c TSXALaCarte.Term, c TSXPrecise.Term) => PerLanguageModes -> (Language, SomeParser c Loc)
jsxParser modes = case jsxMode modes of
  ALaCarte -> jsxParserALaCarte
  Precise  -> jsxParserPrecise

phpParserPrecise :: c PHPPrecise.Term => (Language, SomeParser c Loc)
phpParserPrecise = (PHP, SomeParser (UnmarshalParser @PHPPrecise.Term PHPPrecise.tree_sitter_php))

pythonParserALaCarte :: c PythonALaCarte.Term => (Language, SomeParser c Loc)
pythonParserALaCarte = (Python, SomeParser (AssignmentParser (ASTParser tree_sitter_python) PythonALaCarte.assignment))

pythonParserPrecise :: c PythonPrecise.Term => (Language, SomeParser c Loc)
pythonParserPrecise = (Python, SomeParser (UnmarshalParser @PythonPrecise.Term PythonPrecise.tree_sitter_python))

pythonParser :: (c PythonALaCarte.Term, c PythonPrecise.Term) => PerLanguageModes -> (Language, SomeParser c Loc)
pythonParser modes = case pythonMode modes of
  ALaCarte -> pythonParserALaCarte
  Precise  -> pythonParserPrecise

rubyParserALaCarte :: c RubyALaCarte.Term => (Language, SomeParser c Loc)
rubyParserALaCarte = (Ruby, SomeParser (AssignmentParser (ASTParser tree_sitter_ruby) RubyALaCarte.assignment))

rubyParserPrecise :: c RubyPrecise.Term => (Language, SomeParser c Loc)
rubyParserPrecise = (Ruby, SomeParser (UnmarshalParser @RubyPrecise.Term RubyPrecise.tree_sitter_ruby))

rubyParser :: (c RubyALaCarte.Term, c RubyPrecise.Term) => PerLanguageModes -> (Language, SomeParser c Loc)
rubyParser modes = case rubyMode modes of
  ALaCarte -> rubyParserALaCarte
  Precise  -> rubyParserPrecise

tsxParserALaCarte :: c TSXALaCarte.Term => (Language, SomeParser c Loc)
tsxParserALaCarte = (TSX, SomeParser (AssignmentParser (ASTParser tree_sitter_tsx) TSXALaCarte.assignment))

tsxParserPrecise :: c TSXPrecise.Term => (Language, SomeParser c Loc)
tsxParserPrecise = (TSX, SomeParser (UnmarshalParser @TSXPrecise.Term TSXPrecise.tree_sitter_tsx))

tsxParser :: (c TSXALaCarte.Term, c TSXPrecise.Term) => PerLanguageModes -> (Language, SomeParser c Loc)
tsxParser modes = case tsxMode modes of
  ALaCarte -> tsxParserALaCarte
  Precise  -> tsxParserPrecise

typescriptParserALaCarte :: c TypeScriptALaCarte.Term => (Language, SomeParser c Loc)
typescriptParserALaCarte = (TypeScript, SomeParser (AssignmentParser (ASTParser tree_sitter_typescript) TypeScriptALaCarte.assignment))

typescriptParserPrecise :: c TypeScriptPrecise.Term => (Language, SomeParser c Loc)
typescriptParserPrecise = (TypeScript, SomeParser (UnmarshalParser @TypeScriptPrecise.Term TypeScriptPrecise.tree_sitter_typescript))

typescriptParser :: (c TypeScriptALaCarte.Term, c TypeScriptPrecise.Term) => PerLanguageModes -> (Language, SomeParser c Loc)
typescriptParser modes = case typescriptMode modes of
  ALaCarte -> typescriptParserALaCarte
  Precise  -> typescriptParserPrecise


-- | A type family selecting the language mode for a given term type.
type family TermMode term where
  TermMode GoPrecise.Term         = 'Precise
  TermMode Java.Term              = 'Precise
  TermMode JSON.Term              = 'Precise
  TermMode PHPPrecise.Term        = 'Precise
  TermMode PythonPrecise.Term     = 'Precise
  TermMode RubyPrecise.Term       = 'Precise
  TermMode TypeScriptPrecise.Term = 'Precise
  TermMode TSXPrecise.Term        = 'Precise
  TermMode _                      = 'ALaCarte

-- | The canonical set of parsers producing à la carte terms.
aLaCarteParsers
  :: ( c GoALaCarte.Term
     , c PythonALaCarte.Term
     , c RubyALaCarte.Term
     , c TSXALaCarte.Term
     , c TypeScriptALaCarte.Term
     )
  => Map Language (SomeParser c Loc)
aLaCarteParsers = Map.fromList
  [ javascriptParserALaCarte
  , jsxParserALaCarte
  , pythonParserALaCarte
  , rubyParserALaCarte
  , tsxParserALaCarte
  , typescriptParserALaCarte
  , goParserALaCarte
  ]

-- | The canonical set of parsers producing precise terms.
preciseParsers
  :: ( c Java.Term
     , c JSON.Term
     , c PythonPrecise.Term
     , c RubyPrecise.Term
     , c GoPrecise.Term
     , c PHPPrecise.Term
     , c TypeScriptPrecise.Term
     , c TSXPrecise.Term
     )
  => Map Language (SomeParser c Loc)
preciseParsers = Map.fromList
  [ goParserPrecise
  , javascriptParserPrecise
  , jsonParser
  , jsxParserPrecise
  , pythonParserPrecise
  , phpParserPrecise
  , rubyParserPrecise
  , tsxParserPrecise
  , typescriptParserPrecise
  , javaParser
  ]

-- | The canonical set of all parsers for the passed per-language modes.
allParsers
  :: ( c GoALaCarte.Term
     , c GoPrecise.Term
     , c Java.Term
     , c JSON.Term
     , c PHPPrecise.Term
     , c PythonALaCarte.Term
     , c PythonPrecise.Term
     , c RubyALaCarte.Term
     , c RubyPrecise.Term
     , c TSXALaCarte.Term
     , c TSXPrecise.Term
     , c TypeScriptALaCarte.Term
     , c TypeScriptPrecise.Term
     )
  => PerLanguageModes
  -> Map Language (SomeParser c Loc)
allParsers modes = Map.fromList
  [ goParser modes
  , javaParser
  , javascriptParser modes
  , jsonParser
  , jsxParser modes
  , phpParserPrecise
  , pythonParser modes
  , rubyParser modes
  , tsxParser modes
  , typescriptParser modes
  ]
