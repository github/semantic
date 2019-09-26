{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, GADTs, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Parsing.Parser
( Parser(..)
, SomeTerm(..)
, withSomeTerm
, SomeAnalysisParser(..)
, SomeASTParser(..)
, someASTParser
, someAnalysisParser
, ApplyAll
, ApplyAll'
-- À la carte parsers
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
  -- Precise parsers
, precisePythonParser
) where

import           Assigning.Assignment
import qualified Assigning.Assignment.Deterministic as Deterministic
import qualified CMarkGFM
import           Data.Abstract.Evaluatable (HasPrelude)
import           Data.AST
import           Data.Graph.ControlFlowVertex (VertexDeclaration')
import           Data.Kind
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
import qualified Language.Python.Assignment as Python
import qualified Language.Ruby.Assignment as Ruby
import qualified Language.TSX.Assignment as TSX
import qualified Language.TypeScript.Assignment as TypeScript
import           Prelude hiding (fail)
import           Prologue
import           Source.Range
import           Source.Span
import           TreeSitter.Go
import           TreeSitter.Haskell
import           TreeSitter.JSON
import qualified TreeSitter.Language as TS (Language, Symbol)
import           TreeSitter.PHP
import           TreeSitter.Python
import qualified TreeSitter.Python.AST as Py
import           TreeSitter.Ruby (tree_sitter_ruby)
import           TreeSitter.TSX
import           TreeSitter.TypeScript
import qualified TreeSitter.Node as TS
import           TreeSitter.Unmarshal


type family ApplyAll' (typeclasses :: [(* -> *) -> Constraint]) (fs :: [* -> *]) :: Constraint where
  ApplyAll' (typeclass ': typeclasses) fs = (Apply typeclass fs, ApplyAll' typeclasses fs)
  ApplyAll' '[] fs = ()

-- | A parser, suitable for program analysis, for some specific language, producing 'Term's whose syntax satisfies a list of typeclass constraints.
data SomeAnalysisParser typeclasses ann where
  SomeAnalysisParser :: ( ApplyAll' typeclasses fs
                        , Apply (VertexDeclaration' (Sum fs)) fs
                        , Element Syntax.Identifier fs
                        , HasPrelude lang
                        )
                     => Parser (Term (Sum fs) ann)
                     -> Proxy lang
                     -> SomeAnalysisParser typeclasses ann

-- | A parser for some specific language, producing 'Term's whose syntax satisfies a list of typeclass constraints.
someAnalysisParser :: ( ApplyAll' typeclasses Go.Syntax
                      , ApplyAll' typeclasses PHP.Syntax
                      , ApplyAll' typeclasses Python.Syntax
                      , ApplyAll' typeclasses Ruby.Syntax
                      , ApplyAll' typeclasses TypeScript.Syntax
                      , ApplyAll' typeclasses Haskell.Syntax
                      )
                   => proxy typeclasses                       -- ^ A proxy for the list of typeclasses required, e.g. @(Proxy :: Proxy '[Show1])@.
                   -> Language                                -- ^ The 'Language' to select.
                   -> SomeAnalysisParser typeclasses Loc -- ^ A 'SomeAnalysisParser' abstracting the syntax type to be produced.
someAnalysisParser _ Go         = SomeAnalysisParser goParser         (Proxy :: Proxy 'Go)
someAnalysisParser _ Haskell    = SomeAnalysisParser haskellParser    (Proxy :: Proxy 'Haskell)
someAnalysisParser _ JavaScript = SomeAnalysisParser typescriptParser (Proxy :: Proxy 'JavaScript)
someAnalysisParser _ PHP        = SomeAnalysisParser phpParser        (Proxy :: Proxy 'PHP)
someAnalysisParser _ Python     = SomeAnalysisParser pythonParser     (Proxy :: Proxy 'Python)
someAnalysisParser _ Ruby       = SomeAnalysisParser rubyParser       (Proxy :: Proxy 'Ruby)
someAnalysisParser _ TypeScript = SomeAnalysisParser typescriptParser (Proxy :: Proxy 'TypeScript)
someAnalysisParser _ TSX        = SomeAnalysisParser typescriptParser (Proxy :: Proxy 'TSX)
someAnalysisParser _ l          = error $ "Analysis not supported for: " <> show l


-- | A parser from 'Source' onto some term type.
data Parser term where
  -- | A parser producing 'AST' using a 'TS.Language'.
  ASTParser :: (Bounded grammar, Enum grammar, Show grammar) => Ptr TS.Language -> Parser (AST [] grammar)
  -- | A parser 'Unmarshal'ing to a precise AST type using a 'TS.Language'.
  UnmarshalParser :: Unmarshal t => Ptr TS.Language -> Parser t
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
  -- | An abstraction over parsers when we don’t know the details of the term type.
  SomeParser :: ApplyAll typeclasses syntax => Parser (Term syntax ann) -> Parser (SomeTerm typeclasses ann)

-- | Apply all of a list of typeclasses to all of a list of functors using 'Apply'. Used by 'someParser' to constrain all of the language-specific syntax types to the typeclasses in question.
type family ApplyAll (typeclasses :: [(* -> *) -> Constraint]) (syntax :: * -> *) :: Constraint where
  ApplyAll (typeclass ': typeclasses) syntax = (typeclass syntax, ApplyAll typeclasses syntax)
  ApplyAll '[] syntax = ()


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


precisePythonParser :: Parser (Py.Module Loc)
precisePythonParser = UnmarshalParser tree_sitter_python


data SomeTerm typeclasses ann where
  SomeTerm :: ApplyAll typeclasses syntax => Term syntax ann -> SomeTerm typeclasses ann

withSomeTerm :: (forall syntax . ApplyAll typeclasses syntax => Term syntax ann -> a) -> SomeTerm typeclasses ann -> a
withSomeTerm with (SomeTerm term) = with term

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


-- FIXME: delete these instances once haskell-tree-sitter depends on semantic-source.

instance Unmarshal Loc where
  unmarshalNodes nodes = Loc <$> unmarshalNodes nodes <*> unmarshalNodes nodes

instance Unmarshal Range where
  unmarshalNodes _ = peekNode >>= maybeM (fail "Range expects a current node.") >>= \ node -> do
    let start = fromIntegral (TS.nodeStartByte node)
        end   = fromIntegral (TS.nodeEndByte node)
    pure (Range start end)

instance Unmarshal Span where
  unmarshalNodes _ = peekNode >>= maybeM (fail "Span expects a current node.") >>= \ node -> do
    let start = pointToPos (TS.nodeStartPoint node)
        end   = pointToPos (TS.nodeEndPoint node)
    pure (Span start end)
    where pointToPos (TS.TSPoint line column) = Pos (fromIntegral line) (fromIntegral column)
