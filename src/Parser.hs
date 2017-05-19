{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Parser where

import Info hiding (Go)
import Language
import Prologue
import Source
import Term
import Text.Parser.TreeSitter.C
import Text.Parser.TreeSitter.Go
import Text.Parser.TreeSitter.Ruby
import Text.Parser.TreeSitter.TypeScript
import TreeSitter

data Parser term where
  CParser :: Parser (SyntaxTerm Text DefaultFields)
  GoParser :: Parser (SyntaxTerm Text DefaultFields)
  RubyParser :: Parser (SyntaxTerm Text DefaultFields)
  TypeScriptParser :: Parser (SyntaxTerm Text DefaultFields)

runParser :: Parser term -> SourceBlob -> IO term
runParser parser = case parser of
  CParser -> treeSitterParser C tree_sitter_c
  GoParser -> treeSitterParser Go tree_sitter_go
  RubyParser -> treeSitterParser Ruby tree_sitter_ruby
  TypeScriptParser -> treeSitterParser TypeScript tree_sitter_typescript
