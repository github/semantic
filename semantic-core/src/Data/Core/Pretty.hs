{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings, TypeApplications #-}

module Data.Core.Pretty
  ( showCore
  , printCore
  , showFile
  , printFile
  , prettyCore
  ) where

import           Data.Core
import           Data.File
import           Data.Foldable (toList)
import           Data.Name
import           Data.Scope
import           Data.Stack
import           Data.Term
import           Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.String as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty

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

data Style = Unicode | Ascii

name :: User -> AnsiDoc
name n = if needsQuotation n then enclose (symbol "#{") (symbol "}") (pretty n) else pretty n

prettyCore :: Style -> Term Core User -> AnsiDoc
prettyCore style = precBody . go . fmap name
  where go = \case
          Var v -> atom v
          Term t -> case t of
            Rec (Named (Ignored x) b) -> prec 11 . group . nest 2 $ vsep
              [ keyword "rec" <+> name x
              , symbol "=" <+> align (withPrec 0 (go (instantiate1 (pure (name x)) b)))
              ]

            Lam (Named (Ignored x) b) -> prec 3 . group . nest 2 $ vsep
              [ lambda <> name x, arrow <+> withPrec 3 (go (instantiate1 (pure (name x)) b)) ]

            Record fs -> atom . group . nest 2 $ vsep [ primitive "record", block ", " (map (uncurry keyValue) fs) ]

            Unit     -> atom $ primitive "unit"
            Bool b   -> atom $ primitive (if b then "true" else "false")
            String s -> atom . strlit $ viaShow s

            f :$ x -> prec 8 (withPrec 8 (go f) <+> withPrec 9 (go x))

            If con tru fal -> prec 3 . group $ vsep
              [ keyword "if"   <+> precBody (go con)
              , keyword "then" <+> precBody (go tru)
              , keyword "else" <+> precBody (go fal)
              ]

            Load p -> prec 8 (keyword "load" <+> withPrec 9 (go p))
            item :. body -> prec 9 (withPrec 9 (go item) <> symbol "." <> name body)

            lhs := rhs -> prec 3 . group . nest 2 $ vsep
              [ withPrec 4 (go lhs)
              , symbol "=" <+> align (withPrec 4 (go rhs))
              ]

            -- Annotations are not pretty-printed, as it lowers the signal/noise ratio too profoundly.
            Ann _ c -> go c
            statement ->
              let (bindings, return) = unstatements (Term statement)
                  statements = toList (bindings :> (Nothing :<- return))
                  names = zipWith (\ i (n :<- _) -> maybe (pretty @Int i) (name . namedName) n) [0..] statements
                  statements' = map (prettyStatement names) statements
              in atom (block "; " statements')
        block _ [] = braces mempty
        block s ss = encloseSep "{ " " }" s ss
        keyValue x v = name x <+> symbol ":" <+> precBody (go v)
        prettyStatement names (Just (Named (Ignored u) _) :<- t) = name u <+> arrowL <+> precBody (go (either (names !!) id <$> t))
        prettyStatement names (Nothing                    :<- t) = precBody (go (either (names !!) id <$> t))
        lambda = case style of
          Unicode -> symbol "λ"
          Ascii   -> symbol "\\"
        arrow = case style of
          Unicode -> symbol "→"
          Ascii   -> symbol "->"
        arrowL = case style of
          Unicode -> symbol "←"
          Ascii   -> symbol "<-"


data Prec a = Prec
  { precLevel :: Maybe Int
  , precBody  :: a
  }
  deriving (Eq, Ord, Show)

prec :: Int -> a -> Prec a
prec = Prec . Just

atom :: a -> Prec a
atom = Prec Nothing

withPrec :: Int -> Prec AnsiDoc -> AnsiDoc
withPrec d (Prec d' a)
  | maybe False (d >) d' = parens a
  | otherwise            = a
