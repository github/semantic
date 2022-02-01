-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Syntax
( Syntax(..)
  -- * Pretty-printing
, Print(..)
  -- * Abstract interpretation
, eval0
, eval
, Interpret(..)
  -- * Parsing
, parseFile
, parseGraph
, parseNode
) where

import           Analysis.Effect.Domain
import           Analysis.Effect.Env (Env)
import           Analysis.Effect.Store
import           Control.Applicative (Alternative(..), liftA3)
import           Control.Effect.Labelled
import           Control.Monad (guard)
import qualified Data.Aeson as A
import qualified Data.Aeson.Internal as A
import qualified Data.Aeson.Parser as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as B
import           Data.Function (fix)
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


-- Abstract interpretation

eval0 :: Interpret m i -> m i
eval0 = fix eval

eval :: (Interpret m i -> m i) -> (Interpret m i -> m i)
eval eval (Interpret f) = f eval

newtype Interpret m i = Interpret { interpret :: (Interpret m i -> m i) -> m i }

instance (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m) => Syntax (Interpret m val) where
  iff c t e = Interpret (\ eval -> do
    c' <- eval c
    dif c' (eval t) (eval e))
  noop = Interpret (const dunit)

  bool b = Interpret (\ _ -> dbool b)
  string s = Interpret (\ _ -> dstring s)

  throw e = Interpret (\ eval -> eval e >>= ddie)


-- Parsing

parseFile :: Syntax rep => FilePath -> IO (Either (A.JSONPath, String) (IntMap.IntMap rep))
parseFile path = do
  contents <- B.readFile path
  pure $ A.eitherDecodeWith A.json' (A.iparse parseGraph) contents

parseGraph :: Syntax rep => A.Value -> A.Parser (IntMap.IntMap rep)
parseGraph = A.withArray "nodes" $ \ nodes -> do
  untied <- IntMap.fromList <$> traverse (A.withObject "node" parseNode) (V.toList nodes)
  pure (let tied = ($ tied) <$> untied in tied)

parseNode :: Syntax rep => A.Object -> A.Parser (IntMap.Key, IntMap.IntMap rep -> rep)
parseNode o = do
  edges <- o A..: pack "edges"
  index <- o A..: pack "id"
  let parseType attrs = attrs A..: pack "type" >>= \case
        "string" -> const . string <$> attrs A..: pack "text"
        "true"   -> pure (const (bool True))
        "false"  -> pure (const (bool False))
        "throw"  -> fmap throw <$> edge (head edges)
        "if"     -> liftA3 iff <$> findEdge (edgeNamed "condition") <*> findEdge (edgeNamed "consequence") <*> findEdge (edgeNamed "alternative") <|> pure (const noop)
        "block"  -> pure (const (bool True))
        t        -> A.parseFail ("unrecognized type: " <> t)
      edge = A.withObject "edge" (fmap (flip (IntMap.!)) . (A..: pack "sink"))
      edgeNamed name sink attrs = attrs A..: pack "type" >>= guard . (== name) >> pure (IntMap.! sink)
      findEdge f = foldMap (A.withObject "edge" (\ edge -> do
        sink <- edge A..: pack "sink"
        attrs <- edge A..: pack "attrs"
        f sink attrs)) edges
  o A..: pack "attrs" >>= A.withObject "attrs" (fmap (index,) . parseType)
