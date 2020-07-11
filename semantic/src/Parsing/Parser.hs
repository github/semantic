{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO: Now that a la carte syntax has been removed, this whole abstraction is of perhaps questionable utility

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
, phpParserPrecise
, pythonParser
, codeQLParserPrecise
, rubyParser
, tsxParser
, typescriptParser
  -- * Canonical sets of parsers
, preciseParsers
) where

import           AST.Unmarshal
import           Data.Map (Map)
import qualified Data.Map as Map
import           Foreign.Ptr
import qualified Language.CodeQL as CodeQLPrecise
import qualified Language.Go as GoPrecise
import qualified Language.Java as Java
import qualified Language.JSON as JSON
import qualified Language.PHP as PHPPrecise
import qualified Language.Python as PythonPrecise
import qualified Language.Ruby as RubyPrecise
import qualified Language.TSX as TSXPrecise
import qualified Language.TypeScript as TypeScriptPrecise
import           Prelude hiding (fail)
import           Source.Language (Language (..))
import           Source.Loc
import qualified TreeSitter.Language as TS (Language)

-- | A parser from 'Source' onto some term type.
data Parser term where
  -- | A parser 'Unmarshal'ing to a precise AST type using a 'TS.Language'.
  UnmarshalParser :: Unmarshal t => Ptr TS.Language -> Parser (t Loc)

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

goParser :: c GoPrecise.Term => (Language, SomeParser c Loc)
goParser = (Go, SomeParser (UnmarshalParser @GoPrecise.Term GoPrecise.tree_sitter_go))

javaParser :: c Java.Term => (Language, SomeParser c Loc)
javaParser = (Java, SomeParser (UnmarshalParser @Java.Term Java.tree_sitter_java))

javascriptParser :: c TSXPrecise.Term => (Language, SomeParser c Loc)
javascriptParser = (JavaScript, SomeParser (UnmarshalParser @TSXPrecise.Term TSXPrecise.tree_sitter_tsx))

jsonParser :: c JSON.Term => (Language, SomeParser c Loc)
jsonParser = (JSON, SomeParser (UnmarshalParser @JSON.Term JSON.tree_sitter_json))

jsxParser :: c TSXPrecise.Term => (Language, SomeParser c Loc)
jsxParser = (JSX, SomeParser (UnmarshalParser @TSXPrecise.Term TSXPrecise.tree_sitter_tsx))

phpParserPrecise :: c PHPPrecise.Term => (Language, SomeParser c Loc)
phpParserPrecise = (PHP, SomeParser (UnmarshalParser @PHPPrecise.Term PHPPrecise.tree_sitter_php))

pythonParser :: c PythonPrecise.Term => (Language, SomeParser c Loc)
pythonParser = (Python, SomeParser (UnmarshalParser @PythonPrecise.Term PythonPrecise.tree_sitter_python))

codeQLParserPrecise :: c CodeQLPrecise.Term => (Language, SomeParser c Loc)
codeQLParserPrecise = (CodeQL, SomeParser (UnmarshalParser @CodeQLPrecise.Term CodeQLPrecise.tree_sitter_ql))

rubyParser :: c RubyPrecise.Term => (Language, SomeParser c Loc)
rubyParser = (Ruby, SomeParser (UnmarshalParser @RubyPrecise.Term RubyPrecise.tree_sitter_ruby))

tsxParser :: c TSXPrecise.Term => (Language, SomeParser c Loc)
tsxParser = (TSX, SomeParser (UnmarshalParser @TSXPrecise.Term TSXPrecise.tree_sitter_tsx))

typescriptParser :: c TypeScriptPrecise.Term => (Language, SomeParser c Loc)
typescriptParser = (TypeScript, SomeParser (UnmarshalParser @TypeScriptPrecise.Term TypeScriptPrecise.tree_sitter_typescript))

-- | The canonical set of parsers producing precise terms.
preciseParsers
  :: ( c Java.Term
     , c JSON.Term
     , c PythonPrecise.Term
     , c CodeQLPrecise.Term
     , c RubyPrecise.Term
     , c GoPrecise.Term
     , c PHPPrecise.Term
     , c TypeScriptPrecise.Term
     , c TSXPrecise.Term
     )
  => Map Language (SomeParser c Loc)
preciseParsers = Map.fromList
  [ goParser
  , javascriptParser
  , jsonParser
  , jsxParser
  , pythonParser
  , phpParserPrecise
  , codeQLParserPrecise
  , rubyParser
  , tsxParser
  , typescriptParser
  , javaParser
  ]
