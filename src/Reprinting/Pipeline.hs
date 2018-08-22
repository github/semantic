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
  created /ex nihilo/ during a refactoring operation.
* Generality: each syntax node should have one and only one
  declaration that describes how reprinting takes place. No node
  should be concerned with language-specific syntactic issues.
* Precedence-sensitivity: semantic syntax nodes do not contain
  information about parenthesization of binary operators.
  Binary operators should report their precedence and the
  pipeline should insert parentheses as necessary.
* Component-orientation: each stage in the reprinting pipeline
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
    | Seq Datum
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

{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, RankNTypes #-}
module Reprinting.Pipeline
  ( runReprinter
  , runTokenizing
  , runTranslating
  ) where

import           Control.Monad.Effect as Effect
import qualified Control.Monad.Effect.Exception as Exc
import           Control.Monad.Effect.State
import           Data.Machine hiding (Source)
import           Data.Machine.Runner
import           Data.Record
import           Data.Reprinting.Errors
import           Data.Reprinting.Splice
import           Data.Reprinting.Token
import qualified Data.Source as Source
import           Data.Term
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Reprinting.Tokenize
import           Reprinting.Translate
import           Reprinting.Typeset


-- | Run the reprinting pipeline given the original 'Source', a language
-- specific machine (`ProcessT`) and the provided 'Term'.
runReprinter ::
  ( Show (Record fields)
  , Tokenize a
  , HasField fields History
  )
  => Source.Source
  -> ProcessT Translator Datum Splice
  -> Term a (Record fields)
  -> Either TranslationException Source.Source
runReprinter src languageSubPipeline tree
  = fmap go
  . Effect.run
  . Exc.runError
  . fmap snd
  . runState (mempty :: [Context])
  . foldT $ source (tokenizing src tree)
      ~> translating
      ~> languageSubPipeline
      ~> typesetting
  where go = Source.fromText . renderStrict . layoutPretty defaultLayoutOptions

-- | Run the reprinting pipeline up to tokenizing.
runTokenizing ::
  ( Show (Record fields)
  , Tokenize a
  , HasField fields History
  )
  => Source.Source
  -> Term a (Record fields)
  -> [Token]
runTokenizing src tree
  = Data.Machine.run $ source (tokenizing src tree)

-- | Run the reprinting pipeline up to translating.
runTranslating ::
  ( Show (Record fields)
  , Tokenize a
  , HasField fields History
  )
  => Source.Source
  -> Term a (Record fields)
  -> Either TranslationException [Datum]
runTranslating src tree
  = Effect.run
  . Exc.runError
  . fmap snd
  . runState (mempty :: [Context])
  . runT $ source (tokenizing src tree)
      ~> translating
