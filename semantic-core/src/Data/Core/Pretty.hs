{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings, TypeApplications #-}

module Data.Core.Pretty
  ( showCore
  , printCore
  , showFile
  , printFile
  , prettyCore
  ) where

import           Control.Effect
import           Control.Effect.Reader
import           Data.Core
import           Data.File
import           Data.Name
import           Data.Text.Prettyprint.Doc (Pretty (..), annotate, softline, (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.String as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty

showCore :: Core Name -> String
showCore = Pretty.renderString . Pretty.layoutSmart Pretty.defaultLayoutOptions . Pretty.unAnnotate . prettyCore Ascii

printCore :: Core Name -> IO ()
printCore p = Pretty.putDoc (prettyCore Unicode p) *> putStrLn ""

showFile :: File (Core Name) -> String
showFile = showCore . fileBody

printFile :: File (Core Name) -> IO ()
printFile = printCore . fileBody

type AnsiDoc = Pretty.Doc Pretty.AnsiStyle

keyword, symbol, strlit, primitive :: AnsiDoc -> AnsiDoc
keyword = annotate (Pretty.colorDull Pretty.Cyan)
symbol  = annotate (Pretty.color Pretty.Yellow)
strlit  = annotate (Pretty.colorDull Pretty.Green)
primitive = keyword . mappend "#"

type Prec = Int

data Style = Unicode | Ascii

lambda, arrow :: (Member (Reader Style) sig, Carrier sig m) => m AnsiDoc
lambda = ask @Style >>= \case
  Unicode -> pure $ symbol "λ"
  Ascii   -> pure $ symbol "\\"
arrow = ask @Style >>= \case
  Unicode -> pure $ symbol "→"
  Ascii   -> pure $ symbol "->"

name :: Name -> AnsiDoc
name = \case
  Gen p  -> pretty p
  Path p -> strlit (Pretty.viaShow p)
  User n -> encloseIf (needsQuotation n) (symbol "#{") (symbol "}") (pretty n)

with :: (Member (Reader Prec) sig, Carrier sig m) => Prec -> m a -> m a
with n = local (const n)

inParens :: (Member (Reader Prec) sig, Carrier sig m) => Prec -> m AnsiDoc -> m AnsiDoc
inParens amount go = do
  prec <- ask
  body <- with amount go
  pure (encloseIf (amount >= prec) (symbol "(") (symbol ")") body)

encloseIf :: Monoid m => Bool -> m -> m -> m -> m
encloseIf True  l r x = l <> x <> r
encloseIf False _ _ x = x

prettify :: (Member Naming sig, Member (Reader Prec) sig, Member (Reader Style) sig, Carrier sig m)
         => Core Name
         -> m AnsiDoc
prettify = \case
  Var a -> pure $ name a
  Let a -> pure $ keyword "let" <+> name a
  a :>> b -> do
    prec <- ask @Prec
    fore <- with 12 (prettify a)
    aft  <- with 12 (prettify b)

    let open  = symbol ("{" <> softline)
        close = symbol (softline <> "}")
        separator = ";" <> Pretty.line
        body = fore <> separator <> aft

    pure . Pretty.align $ encloseIf (12 > prec) open close (Pretty.align body)

  Lam f  -> inParens 11 $ do
    x    <- Gen <$> gensym ""
    body <- prettify (instantiate (pure x) f)
    lam  <- lambda
    arr  <- arrow
    pure (lam <> name x <+> arr <+> body)

  Frame    -> pure $ primitive "frame"
  Unit     -> pure $ primitive "unit"
  Bool b   -> pure $ primitive (if b then "true" else "false")
  String s -> pure . strlit $ Pretty.viaShow s

  f :$ x -> inParens 11 $ (<+>) <$> prettify f <*> prettify x

  If con tru fal -> do
    con' <- "if"   `appending` prettify con
    tru' <- "then" `appending` prettify tru
    fal' <- "else" `appending` prettify fal
    pure $ Pretty.sep [con', tru', fal']

  Load p   -> "load" `appending` prettify p
  Edge Lexical n -> "lexical" `appending` prettify n
  Edge Import n -> "import" `appending` prettify n
  item :. body   -> inParens 5 $ do
    f <- prettify item
    g <- prettify body
    pure (f <> symbol "." <> g)

  lhs := rhs -> inParens 4 $ do
    f <- prettify lhs
    g <- prettify rhs
    pure (f <+> symbol "=" <+> g)

  -- Annotations are not pretty-printed, as it lowers the signal/noise ratio too profoundly.
  Ann _ c -> prettify c

appending :: Functor f => AnsiDoc -> f AnsiDoc -> f AnsiDoc
appending k item = (keyword k <+>) <$> item

prettyCore :: Style -> Core Name -> AnsiDoc
prettyCore s = run . runNaming (Root "prettyCore") . runReader @Prec 0 . runReader s . prettify
