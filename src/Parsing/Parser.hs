{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Parsing.Parser
( Parser(..)
, SomeParser(..)
, SomeAnalysisParser(..)
, someParser
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
) where

import           Assigning.Assignment
import qualified CMarkGFM
import           Data.AST
import           Data.Kind
import           Data.Language
import           Data.Record
import qualified Data.Syntax as Syntax
import           Data.Term
import           Data.File
import           Foreign.Ptr
import qualified GHC.TypeLits as TypeLevel
import qualified Language.Go.Assignment as Go
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


type family ApplyAll' (typeclasses :: [(* -> *) -> Constraint]) (fs :: [* -> *]) :: Constraint where
  ApplyAll' (typeclass ': typeclasses) fs = (Apply typeclass fs, ApplyAll' typeclasses fs)
  ApplyAll' '[] fs = ()

-- | A parser, suitable for program analysis, for some specific language, producing 'Term's whose syntax satisfies a list of typeclass constraints.
data SomeAnalysisParser typeclasses ann where
  SomeAnalysisParser :: ( Member Syntax.Identifier fs
                        , ApplyAll' typeclasses fs)
                     => Parser (Term (Union fs) ann) -- ^ A parser.
                     -> Maybe File                   -- ^ Maybe path to prelude.
                     -> SomeAnalysisParser typeclasses ann

-- | A parser for some specific language, producing 'Term's whose syntax satisfies a list of typeclass constraints.
someAnalysisParser :: ( ApplyAll' typeclasses Go.Syntax
                      , ApplyAll' typeclasses Java.Syntax
                      , ApplyAll' typeclasses PHP.Syntax
                      , ApplyAll' typeclasses Python.Syntax
                      , ApplyAll' typeclasses Ruby.Syntax
                      , ApplyAll' typeclasses TypeScript.Syntax
                      )
                   => proxy typeclasses                                -- ^ A proxy for the list of typeclasses required, e.g. @(Proxy :: Proxy '[Show1])@.
                   -> Language                                         -- ^ The 'Language' to select.
                   -> SomeAnalysisParser typeclasses (Record Location) -- ^ A 'SomeAnalysisParser abstracting the syntax type to be produced.
someAnalysisParser _ Go         = SomeAnalysisParser goParser Nothing
someAnalysisParser _ Java       = SomeAnalysisParser javaParser Nothing
someAnalysisParser _ JavaScript = SomeAnalysisParser typescriptParser Nothing
someAnalysisParser _ PHP        = SomeAnalysisParser phpParser Nothing
someAnalysisParser _ Python     = SomeAnalysisParser pythonParser $ Just (File (TypeLevel.symbolVal (Proxy :: Proxy (PreludePath Python.Term))) (Just Python))
someAnalysisParser _ Ruby       = SomeAnalysisParser rubyParser $ Just (File (TypeLevel.symbolVal (Proxy :: Proxy (PreludePath Ruby.Term))) (Just Ruby))
someAnalysisParser _ TypeScript = SomeAnalysisParser typescriptParser Nothing
someAnalysisParser _ l          = error $ "Analysis not supported for: " <> show l


-- | A parser from 'Source' onto some term type.
data Parser term where
  -- | A parser producing 'AST' using a 'TS.Language'.
  ASTParser :: (Bounded grammar, Enum grammar) => Ptr TS.Language -> Parser (AST [] grammar)
  -- | A parser producing an à la carte term given an 'AST'-producing parser and an 'Assignment' onto 'Term's in some syntax type.
  AssignmentParser :: (Enum grammar, Ix grammar, Show grammar, TS.Symbol grammar, Syntax.Error :< fs, Eq1 ast, Apply Foldable fs, Apply Functor fs, Foldable ast, Functor ast)
                   => Parser (Term ast (Node grammar))                           -- A parser producing AST.
                   -> Assignment ast grammar (Term (Union fs) (Record Location)) -- An assignment from AST onto 'Term's.
                   -> Parser (Term (Union fs) (Record Location))                 -- A parser producing 'Term's.
  -- | A parser for 'Markdown' using cmark.
  MarkdownParser :: Parser (Term (TermF [] CMarkGFM.NodeType) (Node Markdown.Grammar))

-- | Apply all of a list of typeclasses to all of a list of functors using 'Apply'. Used by 'someParser' to constrain all of the language-specific syntax types to the typeclasses in question.
type family ApplyAll (typeclasses :: [(* -> *) -> Constraint]) (syntax :: * -> *) :: Constraint where
  ApplyAll (typeclass ': typeclasses) syntax = (typeclass syntax, ApplyAll typeclasses syntax)
  ApplyAll '[] syntax = ()

-- | A parser for some specific language, producing 'Term's whose syntax satisfies a list of typeclass constraints.
--
--   This enables us to abstract over the details of the specific syntax types in cases where we can describe all the requirements on the syntax with a list of typeclasses.
data SomeParser typeclasses ann where
  SomeParser :: ApplyAll typeclasses syntax => Parser (Term syntax ann) -> SomeParser typeclasses ann

-- | Construct a 'SomeParser' given a proxy for a list of typeclasses and the 'Language' to be parsed, all of which must be satisfied by all of the types in the syntaxes of our supported languages.
--
--   This can be used to perform operations uniformly over terms produced by blobs with different 'Language's, and which therefore have different types in general. For example, given some 'Blob', we can parse and 'show' the parsed & assigned 'Term' like so:
--
--   > case someParser (Proxy :: Proxy '[Show1]) <$> blobLanguage language of { Just (SomeParser parser) -> runTask (parse parser blob) >>= putStrLn . show ; _ -> return () }
someParser :: ( ApplyAll typeclasses (Union Go.Syntax)
              , ApplyAll typeclasses (Union Java.Syntax)
              , ApplyAll typeclasses (Union JSON.Syntax)
              , ApplyAll typeclasses (Union Markdown.Syntax)
              , ApplyAll typeclasses (Union Python.Syntax)
              , ApplyAll typeclasses (Union Ruby.Syntax)
              , ApplyAll typeclasses (Union TypeScript.Syntax)
              , ApplyAll typeclasses (Union PHP.Syntax)
              )
           => proxy typeclasses                        -- ^ A proxy for the list of typeclasses required, e.g. @(Proxy :: Proxy '[Show1])@.
           -> Language                                 -- ^ The 'Language' to select.
           -> SomeParser typeclasses (Record Location) -- ^ A 'SomeParser' abstracting the syntax type to be produced.
someParser _ Go         = SomeParser goParser
someParser _ JavaScript = SomeParser typescriptParser
someParser _ Java       = SomeParser javaParser
someParser _ JSON       = SomeParser jsonParser
someParser _ JSX        = SomeParser typescriptParser
someParser _ Markdown   = SomeParser markdownParser
someParser _ Python     = SomeParser pythonParser
someParser _ Ruby       = SomeParser rubyParser
someParser _ TypeScript = SomeParser typescriptParser
someParser _ PHP        = SomeParser phpParser


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

markdownParser :: Parser Markdown.Term
markdownParser = AssignmentParser MarkdownParser Markdown.assignment
