{-# LANGUAGE DeriveTraversable, FlexibleContexts, LambdaCase, OverloadedStrings, RecordWildCards #-}
module Data.Core
( Core(..)
, Edge(..)
, showCore
, lams
, ($$*)
, unapply
, unapplies
, block
, ann
, annWith
) where

import           Control.Applicative (Alternative (..))
import           Data.Foldable (foldl')
import           Data.Loc
import           Data.Name
import           Data.Stack
import           Data.Text.Prettyprint.Doc (Pretty (..), (<+>), vsep)
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.String as Pretty
import           GHC.Stack

data Core
  = Var Name
  | Let Name
  -- | Sequencing without binding; analogous to '>>' or '*>'.
  | Core :>> Core
  | Lam Name Core
  -- | Function application; analogous to '$'.
  | Core :$ Core
  | Unit
  | Bool Bool
  | If Core Core Core
  | String String -- FIXME: Text
  -- | Load the specified file (by path).
  | Load Core
  | Edge Edge Core
  -- | Allocation of a new frame.
  | Frame
  | Core :. Core
  -- | Assignment of a value to the reference returned by the lhs.
  | Core := Core
  | Ann Loc Core
  deriving (Eq, Ord, Show)

infixl 2 :$
infixr 1 :>>
infix  3 :=
infixl 4 :.

data Edge = Lexical | Import
  deriving (Eq, Ord, Show)

instance Pretty Edge where
  pretty = pretty . show

instance Semigroup Core where
  (<>) = (:>>)

softsemi :: Pretty.Doc a
softsemi = Pretty.flatAlt mempty ";"

showCore :: Core -> String
showCore = Pretty.renderString . Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty

instance Pretty Core where
  pretty = \case
    Var a    -> pretty a
    Let a    -> "let" <+> pretty a
    a :>>  b -> vsep [pretty a <> softsemi, pretty b]

    Lam x f  -> vsep [ Pretty.nest 2 $ vsep [ "λ" <> pretty x <+> "-> {"
                                            , pretty f
                                            ]
                     , "}"
                     ]

    f :$ x   -> pretty f <> "." <> pretty x
    Unit     -> Pretty.parens mempty
    Bool b   -> pretty b
    If c x y -> Pretty.sep [ "if" <+> pretty c
                           , "then" <+> pretty x
                           , "else" <+> pretty y
                           ]

    String s -> pretty (show s)

    Frame    -> Pretty.braces mempty

    Load p   -> "load" <+> pretty p
    Edge e n -> pretty e <+> pretty n
    a :. b   -> "push" <+> Pretty.parens (pretty a) <+> Pretty.brackets (pretty b)
    var := x -> pretty var <+> "=" <+> pretty x
    Ann (Loc p s) c  -> pretty c <> Pretty.brackets (pretty p <> ":" <> pretty s)


lams :: Foldable t => t Name -> Core -> Core
lams names body = foldr Lam body names

-- | Application of a function to a sequence of arguments.
($$*) :: Foldable t => Core -> t Core -> Core
($$*) = foldl' (:$)

infixl 9 $$*

unapply :: Alternative m => Core -> m (Core, Core)
unapply (f :$ a) = pure (f, a)
unapply _        = empty

unapplies :: Core -> (Core, Stack Core)
unapplies core = case unapply core of
  Just (f, a) -> (:> a) <$> unapplies f
  Nothing     -> (core, Nil)

block :: Foldable t => t Core -> Core
block cs
  | null cs   = Unit
  | otherwise = foldr1 (:>>) cs

ann :: HasCallStack => Core -> Core
ann = annWith callStack

annWith :: CallStack -> Core -> Core
annWith callStack c = maybe c (flip Ann c) (stackLoc callStack)
