{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings, TypeApplications #-}

module Data.Core.Pretty
  ( showCore
  , printCore
  , showFile
  , printFile
  , prettyCore
  ) where

import           Control.Effect.Reader
import           Data.Core
import           Data.File
import           Data.Name
import           Data.Scope
import           Data.Term
import           Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.String as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import           Data.Traversable (for)

showCore :: Term Core User -> String
showCore = Pretty.renderString . layoutSmart defaultLayoutOptions . unAnnotate . prettyCore Ascii

printCore :: Term Core User -> IO ()
printCore p = Pretty.putDoc (prettyCore Unicode p) *> putStrLn ""

showFile :: File (Term Core User) -> String
showFile = showCore . fileBody

printFile :: File (Term Core User) -> IO ()
printFile = printCore . fileBody

type AnsiDoc = Doc Pretty.AnsiStyle

keyword, symbol, strlit, primitive :: AnsiDoc -> AnsiDoc
keyword = annotate (Pretty.colorDull Pretty.Cyan)
symbol  = annotate (Pretty.color     Pretty.Yellow)
strlit  = annotate (Pretty.colorDull Pretty.Green)
primitive = keyword . mappend "#"

type Prec = Int

data Style = Unicode | Ascii

name :: User -> AnsiDoc
name n = if needsQuotation n then enclose (symbol "#{") (symbol "}") (pretty n) else pretty n

with :: (Member (Reader Prec) sig, Carrier sig m) => Prec -> m a -> m a
with n = local (const n)

inParens :: (Member (Reader Prec) sig, Carrier sig m) => Prec -> m AnsiDoc -> m AnsiDoc
inParens amount go = do
  prec <- ask
  body <- with amount go
  pure (if prec > amount then parens body else body)

inBraces :: (Member (Reader Prec) sig, Carrier sig m) => Prec -> m AnsiDoc -> m AnsiDoc
inBraces amount go = do
  prec <- ask
  body <- with amount go
  pure (if prec > amount then braces (pad body) else body)

pad :: Doc a -> Doc a
pad = enclose space space

prettyCore :: Style -> Term Core User -> AnsiDoc
prettyCore style = run . runReader @Prec 0 . go
  where go = \case
          Var v -> pure (name v)
          Term t -> case t of
            Rec b -> inParens 11 $ do
              (x, body) <- bind b
              pure (keyword "rec" <+> name x <+> symbol "=" <+> body)
            a :>> b -> inBraces 1 $ do
              fore <- with 1 (go a)
              aft  <- with 1 (go b)

              pure $ vsep [ fore <> semi, aft ]

            Named (Ignored x) a :>>= b -> inBraces 1 $ do
              fore <- with 2 (go a)
              aft  <- with 1 (go (instantiate1 (pure x) b))

              pure $ vsep [ name x <+> arrowL <+> fore <> semi, aft ]

            Lam (Named (Ignored x) b) -> inParens 0 $ do
              body <- with 1 (go (instantiate1 (pure x) b))
              pure (lambda <> name x <+> arrow <+> body)

            Record fs -> do
              fs' <- for fs $ \ (x, v) -> (name x <+> symbol "=" <+>) <$> go v
              pure . group . nest 2 $ vsep [ primitive "record", encloseSep "{ " " }" semi fs' ]

            Unit     -> pure $ primitive "unit"
            Bool b   -> pure $ primitive (if b then "true" else "false")
            String s -> pure . strlit $ viaShow s

            f :$ x -> inParens 8 $ (<+>) <$> go f <*> with 9 (go x)

            If con tru fal -> do
              con' <- "if"   `appending` go con
              tru' <- "then" `appending` go tru
              fal' <- "else" `appending` go fal
              pure $ sep [con', tru', fal']

            Load p   -> "load" `appending` go p
            item :. body -> inParens 9 $ do
              f <- go item
              pure (f <> symbol "." <> name body)

            lhs := rhs -> inParens 3 $ do
              f <- go lhs
              g <- go rhs
              pure (f <+> symbol "=" <+> g)

            -- Annotations are not pretty-printed, as it lowers the signal/noise ratio too profoundly.
            Ann _ c -> go c
          where bind (Named (Ignored x) f) = (,) x <$> go (instantiate1 (pure x) f)
        lambda = case style of
          Unicode -> symbol "λ"
          Ascii   -> symbol "\\"
        arrow = case style of
          Unicode -> symbol "→"
          Ascii   -> symbol "->"
        arrowL = case style of
          Unicode -> symbol "←"
          Ascii   -> symbol "<-"
        semi = "; "


appending :: Functor f => AnsiDoc -> f AnsiDoc -> f AnsiDoc
appending k item = (keyword k <+>) <$> item
