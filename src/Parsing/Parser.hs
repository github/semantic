{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, GADTs, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Parsing.Parser
( Parser(..)
, SomeTerm(..)
, withSomeTerm
, SomeSyntaxTerm
, withSomeSyntaxTerm
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
, javaASTParser
, jsonParser
, jsonASTParser
, markdownParser
, pythonParser
, rubyParser
, miniRubyParser
, typescriptParser
, phpParser
, haskellParser
) where

import           Assigning.Assignment
import qualified Assigning.Assignment.Deterministic as Deterministic
import qualified CMarkGFM
import           Data.Abstract.Evaluatable (HasPostlude, HasPrelude)
import           Data.AST
import           Data.Graph.Vertex (VertexDeclaration')
import           Data.Kind
import           Data.Language
import           Data.Record
import           Data.Sum
import qualified Data.Syntax as Syntax
import           Data.Term
import           Foreign.Ptr
import qualified Language.Go.Assignment as Go
import qualified Language.Haskell.Assignment as Haskell
import qualified Language.Java.Assignment as Java
import qualified Language.JSON.Assignment as JSON
import qualified Language.Markdown.Assignment as Markdown
import qualified Language.PHP.Assignment as PHP
import qualified Language.Python.Assignment as Python
import qualified Language.Ruby.Assignment as Ruby
import qualified Language.TypeScript.Assignment as TypeScript
import           Prologue
import           TreeSitter.Go
import           TreeSitter.Haskell
import           TreeSitter.Java
import           TreeSitter.JSON
import qualified TreeSitter.Language as TS (Language, Symbol)
import           TreeSitter.PHP
import           TreeSitter.Python
import           TreeSitter.Ruby
import           TreeSitter.TypeScript


type family ApplyAll' (typeclasses :: [(* -> *) -> Constraint]) (fs :: [* -> *]) :: Constraint where
  ApplyAll' (typeclass ': typeclasses) fs = (Apply typeclass fs, ApplyAll' typeclasses fs)
  ApplyAll' '[] fs = ()

-- | A parser, suitable for program analysis, for some specific language, producing 'Term's whose syntax satisfies a list of typeclass constraints.
data SomeAnalysisParser typeclasses ann where
  SomeAnalysisParser :: ( ApplyAll' typeclasses fs
                        , Apply (VertexDeclaration' (Sum fs)) fs
                        , Element Syntax.Identifier fs
                        , HasPrelude lang
                        , HasPostlude lang
                        )
                     => Parser (Term (Sum fs) ann) -- ^ A parser.
                     -> Proxy lang
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
                   -> SomeAnalysisParser typeclasses (Record Location) -- ^ A 'SomeAnalysisParser' abstracting the syntax type to be produced.
someAnalysisParser _ Go         = SomeAnalysisParser goParser         (Proxy :: Proxy 'Go)
someAnalysisParser _ Haskell    = SomeAnalysisParser haskellParser    (Proxy :: Proxy 'Haskell)
someAnalysisParser _ Java       = SomeAnalysisParser javaParser       (Proxy :: Proxy 'Java)
someAnalysisParser _ JavaScript = SomeAnalysisParser typescriptParser (Proxy :: Proxy 'JavaScript)
someAnalysisParser _ PHP        = SomeAnalysisParser phpParser        (Proxy :: Proxy 'PHP)
someAnalysisParser _ Python     = SomeAnalysisParser pythonParser     (Proxy :: Proxy 'Python)
someAnalysisParser _ Ruby       = SomeAnalysisParser rubyParser       (Proxy :: Proxy 'Ruby)
someAnalysisParser _ TypeScript = SomeAnalysisParser typescriptParser (Proxy :: Proxy 'TypeScript)
someAnalysisParser _ l          = error $ "Analysis not supported for: " <> show l


-- | A parser from 'Source' onto some term type.
data Parser term where
  -- | A parser producing 'AST' using a 'TS.Language'.
  ASTParser :: (Bounded grammar, Enum grammar, Show grammar) => Ptr TS.Language -> Parser (AST [] grammar)
  -- | A parser producing an à la carte term given an 'AST'-producing parser and an 'Assignment' onto 'Term's in some syntax type.
  AssignmentParser :: (Enum grammar, Ix grammar, Show grammar, TS.Symbol grammar, Syntax.Error :< fs, Eq1 ast, Apply Foldable fs, Apply Functor fs, Foldable ast, Functor ast)
                   => Parser (Term ast (Node grammar))                         -- ^ A parser producing AST.
                   -> Assignment ast grammar (Term (Sum fs) (Record Location)) -- ^ An assignment from AST onto 'Term's.
                   -> Parser (Term (Sum fs) (Record Location))                 -- ^ A parser producing 'Term's.
  DeterministicParser :: (Enum grammar, Ord grammar, Show grammar, Element Syntax.Error syntaxes, Apply Foldable syntaxes, Apply Functor syntaxes)
                      => Parser (AST [] grammar)
                      -> Deterministic.Assignment grammar (Term (Sum syntaxes) (Record Location))
                      -> Parser (Term (Sum syntaxes) (Record Location))
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
           => Language                                                -- ^ The 'Language' to select.
           -> Maybe (Parser (SomeTerm typeclasses (Record Location))) -- ^ A 'SomeParser' abstracting the syntax type to be produced.
someParser Go         = Just (SomeParser goParser)
someParser Java       = Just (SomeParser javaParser)
someParser JavaScript = Just (SomeParser typescriptParser)
someParser JSON       = Just (SomeParser jsonParser)
someParser Haskell    = Just (SomeParser haskellParser)
someParser JSX        = Just (SomeParser typescriptParser)
someParser Markdown   = Just (SomeParser markdownParser)
someParser Python     = Just (SomeParser pythonParser)
someParser Ruby       = Just (SomeParser rubyParser)
someParser TypeScript = Just (SomeParser typescriptParser)
someParser PHP        = Just (SomeParser phpParser)
someParser Unknown    = Nothing


goParser :: Parser Go.Term
goParser = AssignmentParser (ASTParser tree_sitter_go) Go.assignment

rubyParser :: Parser Ruby.Term
rubyParser = AssignmentParser (ASTParser tree_sitter_ruby) Ruby.assignment

miniRubyParser :: Parser Ruby.MiniTerm
miniRubyParser = AssignmentParser (ASTParser tree_sitter_ruby) Ruby.miniAssignment

phpParser :: Parser PHP.Term
phpParser = AssignmentParser (ASTParser tree_sitter_php) PHP.assignment

pythonParser :: Parser Python.Term
pythonParser = AssignmentParser (ASTParser tree_sitter_python) Python.assignment

javaParser :: Parser Java.Term
javaParser = AssignmentParser javaASTParser Java.assignment

javaASTParser :: Parser (AST [] Java.Grammar)
javaASTParser = ASTParser tree_sitter_java

jsonParser :: Parser JSON.Term
jsonParser = DeterministicParser jsonASTParser JSON.assignment

jsonASTParser :: Parser (AST [] JSON.Grammar)
jsonASTParser = ASTParser tree_sitter_json

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

data SomeSyntaxTerm syntax ann where
  SomeSyntaxTerm :: Term syntax ann -> SomeSyntaxTerm syntax ann

withSomeSyntaxTerm :: (forall syntax . Term syntax ann -> a) -> SomeSyntaxTerm syntax ann -> a
withSomeSyntaxTerm with (SomeSyntaxTerm term) = with term

-- | A parser for producing specialized (tree-sitter) ASTs.
data SomeASTParser where
  SomeASTParser :: (Bounded grammar, Enum grammar, Show grammar)
                => Parser (AST [] grammar)
                -> SomeASTParser

someASTParser :: Language -> Maybe SomeASTParser
someASTParser Go         = Just (SomeASTParser (ASTParser tree_sitter_go :: Parser (AST [] Go.Grammar)))
someASTParser Haskell    = Just (SomeASTParser (ASTParser tree_sitter_haskell :: Parser (AST [] Haskell.Grammar)))
someASTParser Java       = Just (SomeASTParser (ASTParser tree_sitter_java :: Parser (AST [] Java.Grammar)))
someASTParser JavaScript = Just (SomeASTParser (ASTParser tree_sitter_typescript :: Parser (AST [] TypeScript.Grammar)))
someASTParser JSON       = Just (SomeASTParser (ASTParser tree_sitter_json :: Parser (AST [] JSON.Grammar)))
someASTParser JSX        = Just (SomeASTParser (ASTParser tree_sitter_typescript :: Parser (AST [] TypeScript.Grammar)))
someASTParser Python     = Just (SomeASTParser (ASTParser tree_sitter_python :: Parser (AST [] Python.Grammar)))
someASTParser Ruby       = Just (SomeASTParser (ASTParser tree_sitter_ruby :: Parser (AST [] Ruby.Grammar)))
someASTParser TypeScript = Just (SomeASTParser (ASTParser tree_sitter_typescript :: Parser (AST [] TypeScript.Grammar)))
someASTParser PHP        = Just (SomeASTParser (ASTParser tree_sitter_php :: Parser (AST [] PHP.Grammar)))
someASTParser Markdown   = Nothing
someASTParser Unknown    = Nothing
