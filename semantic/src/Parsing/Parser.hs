{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Parsing.Parser
  ( Parser (..),

    -- * Parsers
    -- $abstract
    SomeParser (..),
    goParser,
    javaParser,
    javascriptParser,
    jsonParser,
    jsxParser,
    phpParser,
    pythonParser,
    codeQLParser,
    rubyParser,
    tsxParser,
    typescriptParser,

    -- * Canonical sets of parsers
    allParsers,
  )
where

import AST.Unmarshal
import Data.Map (Map)
import qualified Data.Map as Map
import Foreign.Ptr
import qualified Language.CodeQL as CodeQL
import qualified Language.Go as Go
import qualified Language.JSON as JSON
import qualified Language.Java as Java
import qualified Language.PHP as PHP
import qualified Language.Python as Python
import qualified Language.Ruby as Ruby
import qualified Language.TSX as TSX
import qualified Language.TypeScript as TypeScript
import Source.Language
import Source.Loc
import qualified TreeSitter.Language as TS (Language)
import Prelude hiding (fail)

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
-- isFancy = parseWith (allParsers @Fancy) (pure . isTermFancy) -- slow compiles!
-- @
--
-- Good:
--
-- @
-- fancyParsers :: 'Map' 'Language' ('SomeParser' Fancy 'Loc')
-- fancyParsers = allParsers
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
goParser = (Go, SomeParser (UnmarshalParser @Go.Term Go.tree_sitter_go))

javaParser :: c Java.Term => (Language, SomeParser c Loc)
javaParser = (Java, SomeParser (UnmarshalParser @Java.Term Java.tree_sitter_java))

javascriptParser :: c TSX.Term => (Language, SomeParser c Loc)
javascriptParser = (JavaScript, SomeParser (UnmarshalParser @TSX.Term TSX.tree_sitter_tsx))

jsonParser :: c JSON.Term => (Language, SomeParser c Loc)
jsonParser = (JSON, SomeParser (UnmarshalParser @JSON.Term JSON.tree_sitter_json))

jsxParser :: c TSX.Term => (Language, SomeParser c Loc)
jsxParser = (JSX, SomeParser (UnmarshalParser @TSX.Term TSX.tree_sitter_tsx))

phpParser :: c PHP.Term => (Language, SomeParser c Loc)
phpParser = (PHP, SomeParser (UnmarshalParser @PHP.Term PHP.tree_sitter_php))

pythonParser :: c Python.Term => (Language, SomeParser c Loc)
pythonParser = (Python, SomeParser (UnmarshalParser @Python.Term Python.tree_sitter_python))

codeQLParser :: c CodeQL.Term => (Language, SomeParser c Loc)
codeQLParser = (CodeQL, SomeParser (UnmarshalParser @CodeQL.Term CodeQL.tree_sitter_ql))

rubyParser :: c Ruby.Term => (Language, SomeParser c Loc)
rubyParser = (Ruby, SomeParser (UnmarshalParser @Ruby.Term Ruby.tree_sitter_ruby))

tsxParser :: c TSX.Term => (Language, SomeParser c Loc)
tsxParser = (TSX, SomeParser (UnmarshalParser @TSX.Term TSX.tree_sitter_tsx))

typescriptParser :: c TypeScript.Term => (Language, SomeParser c Loc)
typescriptParser = (TypeScript, SomeParser (UnmarshalParser @TypeScript.Term TypeScript.tree_sitter_typescript))

-- | The canonical set of parsers producing  terms.
allParsers ::
  ( c Java.Term,
    c JSON.Term,
    c Python.Term,
    c CodeQL.Term,
    c Ruby.Term,
    c Go.Term,
    c PHP.Term,
    c TypeScript.Term,
    c TSX.Term
  ) =>
  Map Language (SomeParser c Loc)
allParsers =
  Map.fromList
    [ goParser,
      javascriptParser,
      jsonParser,
      jsxParser,
      pythonParser,
      phpParser,
      codeQLParser,
      rubyParser,
      tsxParser,
      typescriptParser,
      javaParser
    ]
