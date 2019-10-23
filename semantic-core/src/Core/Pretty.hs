{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings, TypeApplications #-}

module Core.Pretty
  ( showCore
  , printCore
  , showFile
  , printFile
  , prettyCore
  ) where

import           Analysis.File
import           Core.Core
import           Core.Name
import           Data.Foldable (toList)
import           Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.String as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import           Syntax.Pretty
import           Syntax.Scope
import           Syntax.Stack
import           Syntax.Term

showCore :: Term Core Name -> String
showCore = Pretty.renderString . layoutSmart defaultLayoutOptions . unAnnotate . prettyCore Ascii

printCore :: Term Core Name -> IO ()
printCore p = Pretty.putDoc (prettyCore Unicode p) *> putStrLn ""

showFile :: File (Term Core Name) -> String
showFile = showCore . fileBody

printFile :: File (Term Core Name) -> IO ()
printFile = printCore . fileBody

type AnsiDoc = Doc Pretty.AnsiStyle

keyword, symbol, strlit, primitive :: AnsiDoc -> AnsiDoc
keyword = annotate (Pretty.colorDull Pretty.Cyan)
symbol  = annotate (Pretty.color     Pretty.Yellow)
strlit  = annotate (Pretty.colorDull Pretty.Green)
primitive = keyword . mappend "#"

data Style = Unicode | Ascii

name :: Name -> AnsiDoc
name n = if needsQuotation n then enclose (symbol "#{") (symbol "}") (pretty n) else pretty n

prettyCore :: Style -> Term Core Name -> AnsiDoc
prettyCore style = unPrec . go . fmap name
  where go = \case
          Var v -> atom v
          Alg t -> case t of
            Rec (Named (Ignored x) b) -> prec 3 . group . nest 2 $ vsep
              [ keyword "rec" <+> name x
              , symbol "=" <+> align (withPrec 0 (go (instantiate1 (pure (name x)) b)))
              ]

            Lam (Named (Ignored x) b) -> prec 3 . group . nest 2 $ vsep
              [ lambda <> name x, arrow <+> withPrec 0 (go (instantiate1 (pure (name x)) b)) ]

            Record fs -> atom . group . nest 2 $ vsep [ primitive "record", block ", " (map (uncurry keyValue) fs) ]

            Unit     -> atom $ primitive "unit"
            Bool b   -> atom $ primitive (if b then "true" else "false")
            String s -> atom . strlit $ viaShow s

            f :$ x -> prec 8 (withPrec 8 (go f) <+> withPrec 9 (go x))

            If con tru fal -> prec 3 . group $ vsep
              [ keyword "if"   <+> unPrec (go con)
              , keyword "then" <+> unPrec (go tru)
              , keyword "else" <+> unPrec (go fal)
              ]

            Load p -> prec 3 (keyword "load" <+> withPrec 9 (go p))
            item :. body -> prec 9 (withPrec 9 (go item) <> symbol "." <> name body)
            item :? body -> prec 9 (withPrec 9 (go item) <> symbol ".?" <> name body)

            lhs := rhs -> prec 3 . group . nest 2 $ vsep
              [ withPrec 4 (go lhs)
              , symbol "=" <+> align (withPrec 4 (go rhs))
              ]

            statement ->
              let (bindings, return) = unstatements (Alg statement)
                  statements = toList (bindings :> (Nothing :<- return))
                  names = zipWith (\ i (n :<- _) -> maybe (pretty @Int i) (name . namedName) n) [0..] statements
                  statements' = map (prettyStatement names) statements
              in atom (block "; " statements')
        block _ [] = braces mempty
        block s ss = encloseSep "{ " " }" s ss
        keyValue x v = name x <+> symbol ":" <+> unPrec (go v)
        prettyStatement names (Just (Named (Ignored u) _) :<- t) = name u <+> arrowL <+> unPrec (go (either (names !!) id <$> t))
        prettyStatement names (Nothing                    :<- t) = unPrec (go (either (names !!) id <$> t))
        lambda = case style of
          Unicode -> symbol "λ"
          Ascii   -> symbol "\\"
        arrow = case style of
          Unicode -> symbol "→"
          Ascii   -> symbol "->"
        arrowL = case style of
          Unicode -> symbol "←"
          Ascii   -> symbol "<-"
