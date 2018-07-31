module Reprinting.Pipeline ( runReprinter ) where

import Prologue

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import Reprinting.Algebra
import Reprinting.Concrete
import Data.Record
import Data.Term
import Data.Source

runReprinter :: (Reprintable a, HasField fields History, Concrete lang stack)
             => Proxy lang
             -> Source
             -> Term a (Record fields)
             -> Either ConcreteException Source
runReprinter prox s = fmap go . concretize prox . reprint s
  where go = fromText . renderStrict . layoutPretty defaultLayoutOptions
