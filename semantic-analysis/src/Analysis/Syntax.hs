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
  -- * Macro-expressible syntax
, let'
  -- * Parsing
, parseFile
, parseGraph
, parseNode
) where

import           Analysis.Effect.Domain
import           Analysis.Effect.Env (Env, bind)
import           Analysis.Effect.Store
import           Analysis.Name (Name, formatName, nameI)
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
import           Data.Monoid (First(..))
import           Data.Text (Text, pack, unpack)
import qualified Data.Vector as V

class Syntax rep where
  iff :: rep -> rep -> rep -> rep
  noop :: rep

  bool :: Bool -> rep
  string :: Text -> rep

  throw :: rep -> rep

  let_ :: Name -> rep -> (rep -> rep) -> rep


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

  let_ n v b = parens (str "let" <+> name n <+> char '=' <+> v <+> str "in" <+> b (name n))

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

name :: Name -> Print
name = text . formatName


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

  let_ n v b = Interpret (\ eval -> do
    v' <- eval v
    let' n v' (eval (b (Interpret (pure (pure v'))))))


-- Macro-expressible syntax

let' :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m) => Name -> val -> m a -> m a
let' n v m = do
  addr <- alloc n
  addr .= v
  bind n addr m


-- Parsing

parseFile :: Syntax rep => FilePath -> IO (Either (A.JSONPath, String) (Maybe rep))
parseFile path = do
  contents <- B.readFile path
  pure $ snd <$> A.eitherDecodeWith A.json' (A.iparse parseGraph) contents

parseGraph :: Syntax rep => A.Value -> A.Parser (IntMap.IntMap rep, Maybe rep)
parseGraph = A.withArray "nodes" $ \ nodes -> do
  (untied, First root) <- foldMap (\ (k, v, r) -> ([(k, v)], First r)) <$> traverse (A.withObject "node" parseNode) (V.toList nodes)
  let tied = fix (\ tied -> ($ tied) <$> IntMap.fromList untied)
  pure (tied, root)

parseNode :: Syntax rep => A.Object -> A.Parser (IntMap.Key, IntMap.IntMap rep -> rep, Maybe rep)
parseNode o = do
  edges <- o A..: pack "edges"
  index <- o A..: pack "id"
  let parseType attrs = attrs A..: pack "type" >>= \case
        "string" -> const . string <$> attrs A..: pack "text"
        "true"   -> pure (const (bool True))
        "false"  -> pure (const (bool False))
        "throw"  -> fmap throw <$> resolve (head edges)
        "if"     -> liftA3 iff <$> findEdgeNamed "condition" <*> findEdgeNamed "consequence" <*> findEdgeNamed "alternative" <|> pure (const noop)
        "block"  -> fmap (foldr (\ (i, v) r -> let_ (nameI i) v (const r)) noop . zip [0..]) . sequenceA <$> traverse resolve edges
        "module" -> fmap (foldr (\ (i, v) r -> let_ (nameI i) v (const r)) noop . zip [0..]) . sequenceA <$> traverse resolve edges
        t        -> A.parseFail ("unrecognized type: " <> t)
      resolve = resolveWith (const (pure ()))
      resolveWith :: (A.Object -> A.Parser ()) -> A.Value -> A.Parser (IntMap.IntMap rep -> rep)
      resolveWith f = A.withObject "edge" (\ edge -> do
        sink <- edge A..: pack "sink"
        attrs <- edge A..: pack "attrs"
        f attrs
        pure (IntMap.! sink))
      findEdgeNamed name = foldMap (resolveWith (\ attrs -> attrs A..: pack "type" >>= guard . (== name))) edges
  o A..: pack "attrs" >>= A.withObject "attrs" (fmap (index,,Nothing) . parseType)
