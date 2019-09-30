{-# LANGUAGE GADTs, RankNTypes #-}
module Semantic.AST
  ( SomeAST (..)
  , withSomeAST
  , astParseBlob
  , ASTFormat (..)
  , runASTParse
  ) where

import Prologue

import Data.ByteString.Builder
import Data.List (intersperse)

import           Control.Effect.Error
import           Control.Effect.Reader
import           Data.AST
import           Data.Blob
import           Parsing.Parser
import           Rendering.JSON (renderJSONAST)
import           Semantic.Task
import qualified Serializing.Format as F

data SomeAST where
  SomeAST :: Show grammar => AST [] grammar -> SomeAST

withSomeAST :: (forall grammar . Show grammar => AST [] grammar -> a) -> SomeAST -> a
withSomeAST f (SomeAST ast) = f ast

astParseBlob :: (Member (Error SomeException) sig, Member Parse sig, Carrier sig m) => Blob -> m SomeAST
astParseBlob blob@Blob{..}
  | Just (SomeASTParser parser) <- someASTParser (blobLanguage blob) = SomeAST <$> parse parser blob
  | otherwise = noLanguageForBlob (blobPath blob)


data ASTFormat = SExpression | JSON | Show | Quiet
  deriving (Show)

runASTParse :: (Member Distribute sig, Member (Error SomeException) sig, Member Parse sig, Member (Reader TaskSession) sig, Carrier sig m, MonadIO m) => ASTFormat -> [Blob] -> m F.Builder
runASTParse SExpression = distributeFoldMap (astParseBlob >=> withSomeAST (serialize (F.SExpression F.ByShow)))
runASTParse Show        = distributeFoldMap (astParseBlob >=> withSomeAST (serialize F.Show . fmap nodeSymbol))
runASTParse JSON        = distributeFoldMap (\ blob -> withSomeAST (renderJSONAST blob) <$> astParseBlob blob) >=> serialize F.JSON
runASTParse Quiet       = distributeFoldMap $ \blob -> do
  result <- time' ((Right <$> astParseBlob blob) `catchError` (pure . Left @SomeException))
  pure . mconcat . intersperse "\t" $ [ either (const "ERR") (const "OK") (fst result)
                                      , stringUtf8 (show (blobLanguage blob))
                                      , stringUtf8 (blobPath blob)
                                      , doubleDec (snd result) <> " ms\n"
                                      ]
