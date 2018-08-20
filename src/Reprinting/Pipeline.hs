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
  ┌───────────────┬──────────────────────────────────┬────────────────────┐
  │    Module     │           Description            │     Generality     │
  ├───────────────┼──────────────────────────────────┼────────────────────┤
  │    Tokenize   │  A subterm algebra converting    │  Language─agnostic │
  │               │  terms to a stream of tokens.    │                    │
  ├───────────────┼──────────────────────────────────┼────────────────────┤
  │   Translate   │  A stack machine interface       │  Language─specific │
  │               │  through which tokens are        │                    │
  │               │  interpreted to target           │                    │
  │               │  different languages.            │                    │
  ├───────────────┼──────────────────────────────────┼────────────────────┤
  │    Rules      │  A rules engine that informs     │  Language─specific │
  │(unimplemented)│  interpreted Tokens how to lay   │  Project─specific  │
  │               │  themselves out on the page      │                    │
  │               │  with appropriate indentation.   │                    │
  ├───────────────┼──────────────────────────────────┼────────────────────┤
  │    Typeset    │ A simple function informing the  │  Language─agnostic │
  │               │ prettyprinting library how to    │                    │
  │               │ render laid─out tokens into an   │                    │
  │               │ aesthetically pleasing document. │                    │
  └───────────────┴──────────────────────────────────┴────────────────────┘
@

                                       | - extensible, add more steps here...    - |
                                       |                                           |
language agnostic -> language agnostic | -> language specific -> language specific | -> language agnostic
tokenize          -> splice            | -> translate         -> format            | -> typeset
Seq Token         -> Seq Splice        | -> Seq Splice        -> Seq Splice        | -> Doc

-}

{-# LANGUAGE AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables, RankNTypes #-}
module Reprinting.Pipeline ( runReprinter ) where

import           Control.Monad.Effect as Effect
import qualified Control.Monad.Effect.Exception as Exc
import           Control.Monad.Effect.State
import           Data.Machine hiding (Source)
import           Data.Machine.Runner
import           Data.Record
import           Data.Reprinting.Token
import qualified Data.Source as Source
import           Data.Term
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Reprinting.Tokenize
import           Reprinting.Translate
import           Reprinting.Typeset


-- | Given the language of the provided 'Term' and the original 'Source' from
-- which the provided 'Term' was passed, run the reprinting pipeline.
runReprinter ::
  ( Show (Record fields)
  , Tokenize a
  , HasField fields History
  )
  => Source.Source
  -> ProcessT (Eff TranslatingEffs) Splice Splice
  -> Term a (Record fields)
  -> Either TranslationException Source.Source
runReprinter s languageRules tree
  = fmap go
  . Effect.run
  . Exc.runError
  . fmap snd
  . runState (mempty :: [Context])
  . foldT $ source (tokenizing s tree)
      ~> translating
      ~> languageRules
      ~> typesetting
  where go = Source.fromText . renderStrict . layoutPretty defaultLayoutOptions
