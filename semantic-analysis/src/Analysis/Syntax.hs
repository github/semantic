{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Analysis.Syntax
( Syntax(..)
  -- * Pretty-printing
, Print(..)
  -- * Parsing
, parseGraph
, parseNode
) where

import           Control.Applicative (Alternative(..), liftA3)
import           Control.Monad (guard)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.IntMap as IntMap
import           Data.Text (Text, pack, unpack)
import qualified Data.Vector as V

class Syntax rep where
  iff :: rep -> rep -> rep -> rep
  noop :: rep

  bool :: Bool -> rep
  string :: Text -> rep

  throw :: rep -> rep


-- Pretty-printing

newtype Print = Print { print_ :: ShowS }

instance Show Print where
  showsPrec _ = print_

instance Semigroup Print where
  Print a <> Print b = Print (a . b)

instance Monoid Print where
  mempty = Print id

instance Syntax Print where
  iff c t e = parens (str "iff" <+> c <+> str "then" <+> t <+> str "else" <+> e)
  noop = parens (str "noop")

  bool b = parens (str (if b then "true" else "false"))
  string = parens . text

  throw e = parens (str "throw" <+> e)

str :: String -> Print
str = Print . showString

text :: Text -> Print
text = str . unpack

char :: Char -> Print
char = Print . showChar

parens :: Print -> Print
parens p = char '(' <> p <> char ')'

(<+>) :: Print -> Print -> Print
l <+> r = l <> char ' ' <> r

infixr 6 <+>


-- Parsing

parseGraph :: Syntax  rep => A.Value -> A.Parser (IntMap.IntMap rep)
parseGraph = A.withArray "nodes" $ \ nodes -> do
  untied <- IntMap.fromList <$> traverse (A.withObject "node" parseNode) (V.toList nodes)
  pure (let tied = ($ tied) <$> untied in tied)

parseNode :: Syntax rep => A.Object -> A.Parser (IntMap.Key, IntMap.IntMap rep -> rep)
parseNode o = do
  edges <- o A..: pack "edges"
  index <- o A..: pack "index"
  let parseType attrs = attrs A..: pack "type" >>= \case
        "string" -> const . string <$> attrs A..: pack "text"
        "true"   -> pure (const (bool True))
        "false"  -> pure (const (bool False))
        "throw"  -> fmap throw <$> edge (head edges)
        "if"     -> liftA3 iff <$> findEdge (edgeNamed "condition") <*> findEdge (edgeNamed "consequence") <*> findEdge (edgeNamed "alternative") <|> pure (const noop)
        t        -> A.parseFail ("unrecognized type: " <> t)
      edge = A.withObject "edge" (fmap (flip (IntMap.!)) . (A..: pack "sink"))
      edgeNamed name sink attrs = attrs A..: pack "type" >>= guard . (== name) >> pure (IntMap.! sink)
      findEdge f = foldMap (A.withObject "edge" (\ edge -> do
        sink <- edge A..: pack "sink"
        attrs <- edge A..: pack "attrs"
        f sink attrs)) edges
  o A..: pack "attrs" >>= A.withObject "attrs" (fmap (index,) . parseType)
