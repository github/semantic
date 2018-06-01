{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, GADTs, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Parsing.Parser
( Parser(..)
, SomeTerm(..)
, withSomeTerm
, SomeAnalysisParser(..)
, SomeASTParser(..)
, someParser
, someASTParser
, someAnalysisParser
, ApplyAll
, ApplyAll'
-- À la carte parsers
, goParser
, javaParser
, jsonParser
, markdownParser
, pythonParser
, rubyParser
, typescriptParser
, phpParser
, haskellParser
) where

import           Assigning.Assignment
import qualified CMarkGFM
import           Data.AST
import           Data.Kind
import           Data.Language
import           Data.Record
import           Data.Sum
import qualified Data.Syntax as Syntax
import           Data.Term
import           Data.Project
import           Foreign.Ptr
import qualified GHC.TypeLits as TypeLevel
import qualified Language.Go.Assignment as Go
import qualified Language.Haskell.Assignment as Haskell
import qualified Language.Java.Assignment as Java
import qualified Language.JSON.Assignment as JSON
import qualified Language.Markdown.Assignment as Markdown
import qualified Language.PHP.Assignment as PHP
import           Language.Preluded
import qualified Language.Python.Assignment as Python
import qualified Language.Ruby.Assignment as Ruby
import qualified Language.TypeScript.Assignment as TypeScript
import           Prologue
import           TreeSitter.Go
import           TreeSitter.JSON
import qualified TreeSitter.Language as TS (Language, Symbol)
import           TreeSitter.Java
import           TreeSitter.PHP
import           TreeSitter.Python
import           TreeSitter.Ruby
import           TreeSitter.TypeScript
import           TreeSitter.Haskell


type family ApplyAll' (typeclasses :: [(* -> *) -> Constraint]) (fs :: [* -> *]) :: Constraint where
  ApplyAll' (typeclass ': typeclasses) fs = (Apply typeclass fs, ApplyAll' typeclasses fs)
  ApplyAll' '[] fs = ()

-- | A parser, suitable for program analysis, for some specific language, producing 'Term's whose syntax satisfies a list of typeclass constraints.
data SomeAnalysisParser typeclasses ann where
  SomeAnalysisParser :: ( Element Syntax.Identifier fs
                        , ApplyAll' typeclasses fs)
                     => Parser (Term (Sum fs) ann) -- ^ A parser.
                     -> Maybe File                   -- ^ Maybe path to prelude.
                     -> SomeAnalysisParser typeclasses ann

-- | A parser for some specific language, producing 'Term's whose syntax satisfies a list of typeclass constraints.
someAnalysisParser :: ( ApplyAll' typeclasses Go.Syntax
                      , ApplyAll' typeclasses Java.Syntax
                      , ApplyAll' typeclasses PHP.Syntax
                      , ApplyAll' typeclasses Python.Syntax
                      , ApplyAll' typeclasses Ruby.Syntax
                      , ApplyAll' typeclasses TypeScript.Syntax
                      , ApplyAll' typeclasses Haskell.Syntax
                      )
                   => proxy typeclasses                                -- ^ A proxy for the list of typeclasses required, e.g. @(Proxy :: Proxy '[Show1])@.
                   -> Language                                         -- ^ The 'Language' to select.
                   -> SomeAnalysisParser typeclasses (Record Location) -- ^ A 'SomeAnalysisParser abstracting the syntax type to be produced.
someAnalysisParser _ Go         = SomeAnalysisParser goParser Nothing
someAnalysisParser _ Java       = SomeAnalysisParser javaParser Nothing
someAnalysisParser _ JavaScript = SomeAnalysisParser typescriptParser $ Just (File (TypeLevel.symbolVal (Proxy :: Proxy (PreludePath TypeScript.Term))) (Just JavaScript))
someAnalysisParser _ Haskell    = SomeAnalysisParser haskellParser Nothing
someAnalysisParser _ PHP        = SomeAnalysisParser phpParser Nothing
someAnalysisParser _ Python     = SomeAnalysisParser pythonParser $ Just (File (TypeLevel.symbolVal (Proxy :: Proxy (PreludePath Python.Term))) (Just Python))
someAnalysisParser _ Ruby       = SomeAnalysisParser rubyParser $ Just (File (TypeLevel.symbolVal (Proxy :: Proxy (PreludePath Ruby.Term))) (Just Ruby))
someAnalysisParser _ TypeScript = SomeAnalysisParser typescriptParser Nothing
someAnalysisParser _ l          = error $ "Analysis not supported for: " <> show l


-- | A parser from 'Source' onto some term type.
data Parser term where
  -- | A parser producing 'AST' using a 'TS.Language'.
  ASTParser :: (Bounded grammar, Enum grammar, Show grammar) => Ptr TS.Language -> Parser (AST [] grammar)
  -- | A parser producing an à la carte term given an 'AST'-producing parser and an 'Assignment' onto 'Term's in some syntax type.
  AssignmentParser :: (Enum grammar, Ix grammar, Show grammar, TS.Symbol grammar, Syntax.Error :< fs, Eq1 ast, Apply Foldable fs, Apply Functor fs, Foldable ast, Functor ast)
                   => Parser (Term ast (Node grammar))                           -- A parser producing AST.
                   -> Assignment ast grammar (Term (Sum fs) (Record Location)) -- An assignment from AST onto 'Term's.
                   -> Parser (Term (Sum fs) (Record Location))                 -- A parser producing 'Term's.
  -- | A parser for 'Markdown' using cmark.
  MarkdownParser :: Parser (Term (TermF [] CMarkGFM.NodeType) (Node Markdown.Grammar))
  -- | An abstraction over parsers when we don’t know the details of the term type.
  SomeParser :: ApplyAll typeclasses syntax => Parser (Term syntax ann) -> Parser (SomeTerm typeclasses ann)

-- | Apply all of a list of typeclasses to all of a list of functors using 'Apply'. Used by 'someParser' to constrain all of the language-specific syntax types to the typeclasses in question.
type family ApplyAll (typeclasses :: [(* -> *) -> Constraint]) (syntax :: * -> *) :: Constraint where
  ApplyAll (typeclass ': typeclasses) syntax = (typeclass syntax, ApplyAll typeclasses syntax)
  ApplyAll '[] syntax = ()

-- | Construct a 'Parser' given a proxy for a list of typeclasses and the 'Language' to be parsed, all of which must be satisfied by all of the types in the syntaxes of our supported languages.
--
--   This can be used to perform operations uniformly over terms produced by blobs with different 'Language's, and which therefore have different types in general. For example, given some 'Blob', we can parse and 'show' the parsed & assigned 'Term' like so:
--
--   > runTask (parse (someParser @'[Show1] language) blob) >>= putStrLn . withSomeTerm show
someParser :: ( ApplyAll typeclasses (Sum Go.Syntax)
              , ApplyAll typeclasses (Sum Haskell.Syntax)
              , ApplyAll typeclasses (Sum Java.Syntax)
              , ApplyAll typeclasses (Sum JSON.Syntax)
              , ApplyAll typeclasses (Sum Markdown.Syntax)
              , ApplyAll typeclasses (Sum Python.Syntax)
              , ApplyAll typeclasses (Sum Ruby.Syntax)
              , ApplyAll typeclasses (Sum TypeScript.Syntax)
              , ApplyAll typeclasses (Sum PHP.Syntax)
              )
           => Language                                        -- ^ The 'Language' to select.
           -> Parser (SomeTerm typeclasses (Record Location)) -- ^ A 'SomeParser' abstracting the syntax type to be produced.
someParser Go         = SomeParser goParser
someParser Java       = SomeParser javaParser
someParser JavaScript = SomeParser typescriptParser
someParser JSON       = SomeParser jsonParser
someParser Haskell    = SomeParser haskellParser
someParser JSX        = SomeParser typescriptParser
someParser Markdown   = SomeParser markdownParser
someParser Python     = SomeParser pythonParser
someParser Ruby       = SomeParser rubyParser
someParser TypeScript = SomeParser typescriptParser
someParser PHP        = SomeParser phpParser


goParser :: Parser Go.Term
goParser = AssignmentParser (ASTParser tree_sitter_go) Go.assignment

rubyParser :: Parser Ruby.Term
rubyParser = AssignmentParser (ASTParser tree_sitter_ruby) Ruby.assignment

phpParser :: Parser PHP.Term
phpParser = AssignmentParser (ASTParser tree_sitter_php) PHP.assignment

pythonParser :: Parser Python.Term
pythonParser = AssignmentParser (ASTParser tree_sitter_python) Python.assignment

javaParser :: Parser Java.Term
javaParser = AssignmentParser (ASTParser tree_sitter_java) Java.assignment

jsonParser :: Parser JSON.Term
jsonParser = AssignmentParser (ASTParser tree_sitter_json) JSON.assignment

typescriptParser :: Parser TypeScript.Term
typescriptParser = AssignmentParser (ASTParser tree_sitter_typescript) TypeScript.assignment

haskellParser :: Parser Haskell.Term
haskellParser = AssignmentParser (ASTParser tree_sitter_haskell) Haskell.assignment

markdownParser :: Parser Markdown.Term
markdownParser = AssignmentParser MarkdownParser Markdown.assignment


data SomeTerm typeclasses ann where
  SomeTerm :: ApplyAll typeclasses syntax => Term syntax ann -> SomeTerm typeclasses ann

withSomeTerm :: (forall syntax . ApplyAll typeclasses syntax => Term syntax ann -> a) -> SomeTerm typeclasses ann -> a
withSomeTerm with (SomeTerm term) = with term


-- | A parser for producing specialized (tree-sitter) ASTs.
data SomeASTParser where
  SomeASTParser :: (Bounded grammar, Enum grammar, Show grammar)
                => Parser (AST [] grammar)
                -> SomeASTParser

someASTParser :: Language -> SomeASTParser
someASTParser Go         = SomeASTParser (ASTParser tree_sitter_go :: Parser (AST [] Go.Grammar))
someASTParser Haskell    = SomeASTParser (ASTParser tree_sitter_haskell :: Parser (AST [] Haskell.Grammar))
someASTParser JavaScript = SomeASTParser (ASTParser tree_sitter_typescript :: Parser (AST [] TypeScript.Grammar))
someASTParser JSON       = SomeASTParser (ASTParser tree_sitter_json :: Parser (AST [] JSON.Grammar))
someASTParser JSX        = SomeASTParser (ASTParser tree_sitter_typescript :: Parser (AST [] TypeScript.Grammar))
someASTParser Python     = SomeASTParser (ASTParser tree_sitter_python :: Parser (AST [] Python.Grammar))
someASTParser Ruby       = SomeASTParser (ASTParser tree_sitter_ruby :: Parser (AST [] Ruby.Grammar))
someASTParser TypeScript = SomeASTParser (ASTParser tree_sitter_typescript :: Parser (AST [] TypeScript.Grammar))
someASTParser PHP        = SomeASTParser (ASTParser tree_sitter_php :: Parser (AST [] PHP.Grammar))
someASTParser l          = error $ "Tree-Sitter AST parsing not supported for: " <> show l
