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
import           Data.Functor.Foldable
import           Data.Name
import           Data.Text.Prettyprint.Doc (Pretty (..), annotate, softline, (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.String as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty

showCore :: Core -> String
showCore = Pretty.renderString . Pretty.layoutSmart Pretty.defaultLayoutOptions . Pretty.unAnnotate . prettyCore Ascii

printCore :: Core -> IO ()
printCore p = Pretty.putDoc (prettyCore Unicode p) *> putStrLn ""

showFile :: File Core -> String
showFile = showCore . fileBody

printFile :: File Core -> IO ()
printFile = printCore . fileBody

type AnsiDoc = Pretty.Doc Pretty.AnsiStyle

keyword, symbol, strlit, primitive :: AnsiDoc -> AnsiDoc
keyword = annotate (Pretty.colorDull Pretty.Cyan)
symbol  = annotate (Pretty.color Pretty.Yellow)
strlit  = annotate (Pretty.colorDull Pretty.Green)
primitive = keyword . mappend "#"

type Prec    = Int

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
  Path p -> strlit (Pretty.viaShow p  )
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

prettify :: (Member (Reader Prec) sig, Member (Reader Style) sig, Carrier sig m)
         => CoreF (m AnsiDoc)
         -> m AnsiDoc
prettify = \case
  VarF a -> pure $ name a
  LetF a -> pure $ keyword "let" <+> name a
  a :>>$ b -> do
    prec <- ask @Prec
    fore <- with 12 a
    aft  <- with 12 b

    let open  = symbol (if 12 > prec then "{" <> softline else "")
        close = symbol (if 12 > prec then softline <> "}" else "")
        separator = ";" <> Pretty.line
        body = fore <> separator <> aft

    pure . Pretty.align $ open <> Pretty.align body <> close

  LamF x f  -> inParens 11 $ do
    body <- f
    lam  <- lambda
    arr  <- arrow
    pure (lam <> name x <+> arr <+> body)

  FrameF    -> pure $ primitive "frame"
  UnitF     -> pure $ primitive "unit"
  BoolF b   -> pure $ primitive (if b then "true" else "false")
  StringF s -> pure . strlit $ Pretty.viaShow s

  f :$$ x -> inParens 11 $ (<+>) <$> f <*> x

  IfF con tru fal -> do
    con' <- "if"   `appending` con
    tru' <- "then" `appending` tru
    fal' <- "else" `appending` fal
    pure $ Pretty.sep [con', tru', fal']

  LoadF p   -> "load" `appending` p
  EdgeF Lexical n -> "lexical" `appending` n
  EdgeF Import n -> "import" `appending` n
  item :.$ body   -> inParens 5 $ do
    f <- item
    g <- body
    pure (f <> symbol "." <> g)

  lhs :=$ rhs -> inParens 4 $ do
    f <- lhs
    g <- rhs
    pure (f <+> symbol "=" <+> g)

  -- Annotations are not pretty-printed, as it lowers the signal/noise ratio too profoundly.
  AnnF _ c -> c

appending :: Functor f => AnsiDoc -> f AnsiDoc -> f AnsiDoc
appending k item = (keyword k <+>) <$> item

prettyCore :: Style -> Core -> AnsiDoc
prettyCore s = run . runReader @Prec 0 . runReader s . cata prettify
