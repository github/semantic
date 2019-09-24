{- |

This module represents the top-level interface for @semantic@'s
reprinting functionality. Reprinting here is defined as the
conversion, given some 'Source' code, of the parsed (and possibly
modified) syntax tree corresponding to that source, back into a
document representing that source code.

The approach is based on techniques drawn from:

* /A Pretty Good Formatting Pipeline/ by Bagge and Hasu (2010)
* /Scrap Your Reprinter/ by Orchard et al (2017)

The reprinter was designed with the following goals in mind:

* Laziness: a node that was unmodified in a refactoring pass
  should draw its representation from the original source file,
  rather than being explicitly pretty-printed. The only nodes
  that do not consult the original document are those that were
  synthesized during a refactoring operation.
* Generality: each syntax node should have one and only one
  declaration that describes how reprinting takes place. No node
  should be concerned with language-specific syntactic issues.
* Precedence-sensitivity: semantic syntax nodes do not contain
  information about parenthesization of binary operators.
  Binary operators should report their precedence and the
  pipeline should insert parentheses as necessary.
* Modularity: each stage in the reprinting pipeline
  should be testable independently.
* Time/space efficiency: the reprinting algorithm should scale
  to trees with hundreds of thousands of nodes without linear
  space usage.
* Roundtrip correctness: reprinting an unmodified syntax tree
  should produce source text exactly corresponding to the original
  file.

The reprinter takes the form of a pipeline operating over a stream of
tokens. Each stage in the pipeline converts a given token to a
lower-level representation, ultimately resulting in a 'Doc' data type
from the @prettyprinter@ library (to which we defer the nitty-gritty
details of actually pretty-printing text).  A representation of the
stages of the pipeline follows:

@

[Start]
  The Pipeline starts with a tree, where terms are annotated with 'History' to
  denote what's been refactored.
  (Language-agnostic)
    |
    | AST
    |
    v
[Tokenize]
  A subterm algebra converting a tree (terms) to a stream of tokens.
  (Language-agnostic)
    |
    | Seq Token
    |
    v
[Translate]
  A stack machine interface through which tokens are interpreted to splices
  (with context). A splice is a concrete representation of syntax, to which
  additional language specific transformations can be applied.
  (Language-agnostic)
    |
    | Seq Fragment
    |
    v
[PrettyPrint] --> <Format> --> <Beautify> --> <...>
  A language specific stack machine interface allowing further refinement of the
  sequence of splices. Language machines should emit specific keywords,
  punctutation, and layout rules. Additional steps can be added for project
  specific style, formatting, and even post-processing (minimizers, etc).
  (Language-specific, Project-specific)
    |
    | Seq Splice
    |
    v
[Typeset]
  A stack machine that converts splices to a Doc. (Language-agnostic)
    |
    | Doc
    |
    v
[Print]
  A simple function that produces 'Text' or 'Source' with the desired layout
  settings from a 'Doc'. (Language-agnostic)
    |
    | Text
    |
    v

@

-}

{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, ScopedTypeVariables #-}
module Reprinting.Pipeline
  ( runReprinter
  , runTokenizing
  , runContextualizing
  , runTranslating
  ) where

import           Control.Effect as Effect
import           Control.Effect.Error as Effect
import           Control.Effect.State as Effect
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Streaming
import qualified Streaming.Prelude as Streaming

import           Data.Reprinting.Errors
import           Data.Reprinting.Scope
import           Data.Reprinting.Splice
import           Data.Reprinting.Token
import           Data.Term
import           Reprinting.Tokenize
import           Reprinting.Translate
import           Reprinting.Typeset
import qualified Source.Source as Source

-- | Run the reprinting pipeline given the original 'Source', a language specific
-- translation function (as a function over 'Stream's) and the provided 'Term'.
runReprinter :: Tokenize a
             => Source.Source
             -> (Stream (Of Fragment) TranslatorC () -> Stream (Of Splice) TranslatorC ())
             -> Term a History
             -> Either TranslationError Source.Source
runReprinter src translating
  = fmap go
  . Effect.run
  . Effect.runError
  . evalState @[Scope] mempty
  . Streaming.mconcat_
  . typesetting
  . translating
  . contextualizing
  . tokenizing src
  where go = Source.fromText . renderStrict . layoutPretty defaultLayoutOptions

-- | Run the reprinting pipeline up to tokenizing.
runTokenizing :: Tokenize a
              => Source.Source
              -> Term a History
              -> [Token]
runTokenizing src
  = runIdentity
  . Streaming.toList_
  . tokenizing src

-- | Run the reprinting pipeline up to contextualizing.
runContextualizing :: Tokenize a
                   => Source.Source
                   -> Term a History
                   -> Either TranslationError [Fragment]
runContextualizing src
  = Effect.run
  . Effect.runError
  . evalState @[Scope] mempty
  . Streaming.toList_
  . contextualizing
  . tokenizing src

runTranslating :: Tokenize a
               => Source.Source
               -> (Stream (Of Fragment) TranslatorC () -> Stream (Of Splice) TranslatorC ())
               -> Term a History
               -> Either TranslationError [Splice]
runTranslating src translating
  = Effect.run
  . Effect.runError
  . evalState @[Scope] mempty
  . Streaming.toList_
  . translating
  . contextualizing
  . tokenizing src
