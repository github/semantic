{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Parser where

import Info hiding (Go)
import Language
import Prologue
import Source
import Term

data Parser term where
  CParser :: Parser (SyntaxTerm Text DefaultFields)
  GoParser :: Parser (SyntaxTerm Text DefaultFields)
  RubyParser :: Parser (SyntaxTerm Text DefaultFields)
  TypeScriptParser :: Parser (SyntaxTerm Text DefaultFields)
