{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, GADTs, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Parsing.Parser
( Parser(..)
, SomeAnalysisParser(..)
, SomeASTParser(..)
, someASTParser
, someAnalysisParser
-- * À la carte parsers
, goParser
, goASTParser
, jsonParser
, jsonASTParser
, markdownParser
, pythonParser
, pythonASTParser
, rubyParser
, tsxParser
, typescriptParser
, typescriptASTParser
, phpParser
, phpASTParser
, haskellParser
  -- * Abstract parsers
, SomeParser(..)
, goParser'
, haskellParser'
, javascriptParser'
, jsonParser'
, jsxParser'
, markdownParser'
, phpParser'
, pythonParserALaCarte'
, pythonParserPrecise'
, pythonParser'
, rubyParser'
, tsxParser'
, typescriptParser'
  -- * Canonical sets of parsers
, aLaCarteParsers
, preciseParsers
, allParsers
) where

import           Assigning.Assignment
import qualified Assigning.Assignment.Deterministic as Deterministic
import qualified CMarkGFM
import           Data.Abstract.Evaluatable (HasPrelude)
import           Data.AST
import           Data.Graph.ControlFlowVertex (VertexDeclaration')
import           Data.Language
import           Data.Sum
import qualified Data.Syntax as Syntax
import           Data.Term
import           Foreign.Ptr
import qualified Language.Go.Assignment as Go
import qualified Language.Haskell.Assignment as Haskell
import qualified Language.JSON.Assignment as JSON
import qualified Language.Markdown.Assignment as Markdown
import qualified Language.PHP.Assignment as PHP
import qualified Language.Python as Py
import qualified Language.Python.Assignment as Python
import qualified Language.Ruby.Assignment as Ruby
import qualified Language.TSX.Assignment as TSX
import qualified Language.TypeScript.Assignment as TypeScript
import           Prelude hiding (fail)
import           Prologue
import           TreeSitter.Go
import           TreeSitter.Haskell
import           TreeSitter.JSON
import qualified TreeSitter.Language as TS (Language, Symbol)
import           TreeSitter.PHP
import           TreeSitter.Python
import           TreeSitter.Ruby (tree_sitter_ruby)
import           TreeSitter.TSX
import           TreeSitter.TypeScript
import           TreeSitter.Unmarshal


-- | A parser, suitable for program analysis, for some specific language, producing 'Term's whose syntax satisfies a list of typeclass constraints.
data SomeAnalysisParser constraint ann where
  SomeAnalysisParser :: ( constraint (Sum fs)
                        , Apply (VertexDeclaration' (Sum fs)) fs
                        , HasPrelude lang
                        )
                     => Parser (Term (Sum fs) ann)
                     -> Proxy lang
                     -> SomeAnalysisParser constraint ann

-- | A parser for some specific language, producing 'Term's whose syntax satisfies a list of typeclass constraints.
someAnalysisParser :: ( constraint (Sum Go.Syntax)
                      , constraint (Sum PHP.Syntax)
                      , constraint (Sum Python.Syntax)
                      , constraint (Sum Ruby.Syntax)
                      , constraint (Sum TypeScript.Syntax)
                      , constraint (Sum Haskell.Syntax)
                      )
                   => proxy constraint                  -- ^ A proxy for the list of typeclasses required, e.g. @(Proxy :: Proxy '[Show1])@.
                   -> Language                          -- ^ The 'Language' to select.
                   -> SomeAnalysisParser constraint Loc -- ^ A 'SomeAnalysisParser' abstracting the syntax type to be produced.
someAnalysisParser _ Go         = SomeAnalysisParser goParser         (Proxy @'Go)
someAnalysisParser _ Haskell    = SomeAnalysisParser haskellParser    (Proxy @'Haskell)
someAnalysisParser _ JavaScript = SomeAnalysisParser typescriptParser (Proxy @'JavaScript)
someAnalysisParser _ PHP        = SomeAnalysisParser phpParser        (Proxy @'PHP)
someAnalysisParser _ Python     = SomeAnalysisParser pythonParser     (Proxy @'Python)
someAnalysisParser _ Ruby       = SomeAnalysisParser rubyParser       (Proxy @'Ruby)
someAnalysisParser _ TypeScript = SomeAnalysisParser typescriptParser (Proxy @'TypeScript)
someAnalysisParser _ TSX        = SomeAnalysisParser typescriptParser (Proxy @'TSX)
someAnalysisParser _ l          = error $ "Analysis not supported for: " <> show l


-- | A parser from 'Source' onto some term type.
data Parser term where
  -- | A parser producing 'AST' using a 'TS.Language'.
  ASTParser :: (Bounded grammar, Enum grammar, Show grammar) => Ptr TS.Language -> Parser (AST [] grammar)
  -- | A parser 'Unmarshal'ing to a precise AST type using a 'TS.Language'.
  UnmarshalParser :: Unmarshal t => Ptr TS.Language -> Parser (t Loc)
  -- | A parser producing an à la carte term given an 'AST'-producing parser and an 'Assignment' onto 'Term's in some syntax type.
  AssignmentParser :: (Enum grammar, Ix grammar, Show grammar, TS.Symbol grammar, Syntax.Error :< fs, Eq1 ast, Apply Foldable fs, Apply Functor fs, Foldable ast, Functor ast)
                   => Parser (Term ast (Node grammar))           -- ^ A parser producing AST.
                   -> Assignment ast grammar (Term (Sum fs) Loc) -- ^ An assignment from AST onto 'Term's.
                   -> Parser (Term (Sum fs) Loc)                 -- ^ A parser producing 'Term's.
  DeterministicParser :: (Enum grammar, Ord grammar, Show grammar, Element Syntax.Error syntaxes, Apply Foldable syntaxes, Apply Functor syntaxes)
                      => Parser (AST [] grammar)
                      -> Deterministic.Assignment grammar (Term (Sum syntaxes) Loc)
                      -> Parser (Term (Sum syntaxes) Loc)
  -- | A parser for 'Markdown' using cmark.
  MarkdownParser :: Parser (Term (TermF [] CMarkGFM.NodeType) (Node Markdown.Grammar))


goParser :: Parser Go.Term
goParser = AssignmentParser (ASTParser tree_sitter_go) Go.assignment

goASTParser :: Parser (AST [] Go.Grammar)
goASTParser = ASTParser tree_sitter_go

rubyParser :: Parser Ruby.Term
rubyParser = AssignmentParser (ASTParser tree_sitter_ruby) Ruby.assignment

phpParser :: Parser PHP.Term
phpParser = AssignmentParser (ASTParser tree_sitter_php) PHP.assignment

phpASTParser :: Parser (AST [] PHP.Grammar)
phpASTParser = ASTParser tree_sitter_php

pythonParser :: Parser Python.Term
pythonParser = AssignmentParser (ASTParser tree_sitter_python) Python.assignment

pythonASTParser :: Parser (AST [] Python.Grammar)
pythonASTParser = ASTParser tree_sitter_python

jsonParser :: Parser JSON.Term
jsonParser = DeterministicParser jsonASTParser JSON.assignment

jsonASTParser :: Parser (AST [] JSON.Grammar)
jsonASTParser = ASTParser tree_sitter_json

typescriptParser :: Parser TypeScript.Term
typescriptParser = AssignmentParser (ASTParser tree_sitter_typescript) TypeScript.assignment

tsxParser :: Parser TSX.Term
tsxParser = AssignmentParser (ASTParser tree_sitter_tsx) TSX.assignment

typescriptASTParser :: Parser (AST [] TypeScript.Grammar)
typescriptASTParser = ASTParser tree_sitter_typescript

haskellParser :: Parser Haskell.Term
haskellParser = AssignmentParser (ASTParser tree_sitter_haskell) Haskell.assignment

markdownParser :: Parser Markdown.Term
markdownParser = AssignmentParser MarkdownParser Markdown.assignment


pythonParserPrecise :: Parser (Py.Term Loc)
pythonParserPrecise = UnmarshalParser tree_sitter_python


-- | A parser for producing specialized (tree-sitter) ASTs.
data SomeASTParser where
  SomeASTParser :: (Bounded grammar, Enum grammar, Show grammar)
                => Parser (AST [] grammar)
                -> SomeASTParser

someASTParser :: Language -> Maybe SomeASTParser
someASTParser Go         = Just (SomeASTParser (ASTParser tree_sitter_go :: Parser (AST [] Go.Grammar)))
someASTParser Haskell    = Just (SomeASTParser (ASTParser tree_sitter_haskell :: Parser (AST [] Haskell.Grammar)))
someASTParser JSON       = Just (SomeASTParser (ASTParser tree_sitter_json :: Parser (AST [] JSON.Grammar)))

-- Use the TSX parser for `.js` and `.jsx` files in case they use Flow type-annotation syntax.
-- The TSX and Flow syntaxes are the same, whereas the normal TypeScript syntax is different.
someASTParser JavaScript = Just (SomeASTParser (ASTParser tree_sitter_tsx :: Parser (AST [] TSX.Grammar)))
someASTParser JSX        = Just (SomeASTParser (ASTParser tree_sitter_tsx :: Parser (AST [] TSX.Grammar)))

someASTParser Python     = Just (SomeASTParser (ASTParser tree_sitter_python :: Parser (AST [] Python.Grammar)))
someASTParser Ruby       = Just (SomeASTParser (ASTParser tree_sitter_ruby :: Parser (AST [] Ruby.Grammar)))
someASTParser TypeScript = Just (SomeASTParser (ASTParser tree_sitter_typescript :: Parser (AST [] TypeScript.Grammar)))
someASTParser TSX        = Just (SomeASTParser (ASTParser tree_sitter_tsx :: Parser (AST [] TSX.Grammar)))
someASTParser PHP        = Just (SomeASTParser (ASTParser tree_sitter_php :: Parser (AST [] PHP.Grammar)))
someASTParser Java       = Nothing
someASTParser Markdown   = Nothing
someASTParser Unknown    = Nothing


data SomeParser c a where
  SomeParser :: c t => Parser (t a) -> SomeParser c a

goParser' :: c (Term (Sum Go.Syntax)) => (Language, SomeParser c Loc)
goParser' = (Go, SomeParser goParser)

haskellParser' :: c (Term (Sum Haskell.Syntax)) => (Language, SomeParser c Loc)
haskellParser' = (Haskell, SomeParser haskellParser)

javascriptParser' :: c (Term (Sum TSX.Syntax)) => (Language, SomeParser c Loc)
javascriptParser' = (JavaScript, SomeParser tsxParser)

jsonParser' :: c (Term (Sum JSON.Syntax)) => (Language, SomeParser c Loc)
jsonParser' = (JSON, SomeParser jsonParser)

jsxParser' :: c (Term (Sum TSX.Syntax)) => (Language, SomeParser c Loc)
jsxParser' = (JSX, SomeParser tsxParser)

markdownParser' :: c (Term (Sum Markdown.Syntax)) => (Language, SomeParser c Loc)
markdownParser' = (Markdown, SomeParser markdownParser)

phpParser' :: c (Term (Sum PHP.Syntax)) => (Language, SomeParser c Loc)
phpParser' = (PHP, SomeParser phpParser)

pythonParserALaCarte' :: c (Term (Sum Python.Syntax)) => (Language, SomeParser c Loc)
pythonParserALaCarte' = (Python, SomeParser pythonParser)

pythonParserPrecise' :: c Py.Term => (Language, SomeParser c Loc)
pythonParserPrecise' = (Python, SomeParser pythonParserPrecise)

pythonParser' :: (c (Term (Sum Python.Syntax)), c Py.Term) => PerLanguageModes -> (Language, SomeParser c Loc)
pythonParser' modes = case pythonMode modes of
  ALaCarte -> (Python, SomeParser pythonParser)
  Precise  -> (Python, SomeParser pythonParserPrecise)

rubyParser' :: c (Term (Sum Ruby.Syntax)) => (Language, SomeParser c Loc)
rubyParser' = (Ruby, SomeParser rubyParser)

tsxParser' :: c (Term (Sum TSX.Syntax)) => (Language, SomeParser c Loc)
tsxParser' = (TSX, SomeParser tsxParser)

typescriptParser' :: c (Term (Sum TypeScript.Syntax)) => (Language, SomeParser c Loc)
typescriptParser' = (TypeScript, SomeParser typescriptParser)


aLaCarteParsers
  :: ( c (Term (Sum Go.Syntax))
     , c (Term (Sum Haskell.Syntax))
     , c (Term (Sum JSON.Syntax))
     , c (Term (Sum Markdown.Syntax))
     , c (Term (Sum PHP.Syntax))
     , c (Term (Sum Python.Syntax))
     , c (Term (Sum Ruby.Syntax))
     , c (Term (Sum TSX.Syntax))
     , c (Term (Sum TypeScript.Syntax))
     )
  => [(Language, SomeParser c Loc)]
aLaCarteParsers =
  [ goParser'
  , haskellParser'
  , javascriptParser'
  , jsonParser'
  , jsxParser'
  , markdownParser'
  , phpParser'
  , pythonParserALaCarte'
  , rubyParser'
  , typescriptParser'
  , tsxParser'
  ]

preciseParsers :: c Py.Term => [(Language, SomeParser c Loc)]
preciseParsers =
  [ pythonParserPrecise'
  ]

allParsers
  :: ( c (Term (Sum Go.Syntax))
     , c (Term (Sum Haskell.Syntax))
     , c (Term (Sum JSON.Syntax))
     , c (Term (Sum Markdown.Syntax))
     , c (Term (Sum PHP.Syntax))
     , c (Term (Sum Python.Syntax))
     , c Py.Term
     , c (Term (Sum Ruby.Syntax))
     , c (Term (Sum TSX.Syntax))
     , c (Term (Sum TypeScript.Syntax))
     )
  => PerLanguageModes
  -> [(Language, SomeParser c Loc)]
allParsers modes =
  [ goParser'
  , haskellParser'
  , javascriptParser'
  , jsonParser'
  , jsxParser'
  , markdownParser'
  , phpParser'
  , pythonParser' modes
  , rubyParser'
  , typescriptParser'
  , tsxParser'
  ]
