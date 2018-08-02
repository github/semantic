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
* Component-orientation: each stage in the reprinting pipeline
  should be testable independently
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
  │   Algebraic   │  A subterm algebra converting    │  Language─agnostic │
  │               │  terms to a stream of tokens.    │                    │
  ├───────────────┼──────────────────────────────────┼────────────────────┤
  │   Concrete    │  A stack machine interface       │  Language─specific │
  │               │  through which tokens are        │                    │
  │               │  interpreted to target           │                    │
  │               │  different languages.            │                    │
  ├───────────────┼──────────────────────────────────┼────────────────────┤
  │   Layout      │  A rules engine that informs     │  Language─specific │
  │(unimplemented)│  interpreted Tokens how to lay   │  Project─specific  │
  │               │  themselves out on the page      │                    │
  │               │  with appropriate indentation.   │                    │
  ├───────────────┼──────────────────────────────────┼────────────────────┤
  │   Typeset     │ A stack machine informing the    │  Language─agnostic │
  │(unimplemented)│ prettyprinting library how to    │                    │
  │               │ render laid─out tokens into an   │                    │
  │               │ aesthetically pleasing document. │                    │
  │               │                                  │                    │
  └───────────────┴──────────────────────────────────┴────────────────────┘
@

-}

module Reprinting.Pipeline ( runReprinter ) where

import Prologue

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import Reprinting.Algebraic
import Reprinting.Concrete
import Data.Record
import Data.Term
import Data.Source

-- | Given a 'Proxy' corresponding to the language of the provided
-- 'Term' and the original 'Source' from which the provided 'Term' was
-- passed, run the reprinting pipeline.
runReprinter :: (Show (Record fields), Reprintable a, HasField fields History, Concrete lang, Lower (Stack lang))
             => Proxy lang
             -> Source
             -> Term a (Record fields)
             -> Either ConcreteException Source
runReprinter prox s = fmap go . concretize prox . reprint s
  where go = fromText . renderStrict . layoutPretty defaultLayoutOptions
