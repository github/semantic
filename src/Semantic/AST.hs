{-# LANGUAGE GADTs, RankNTypes #-}
module Semantic.AST
  ( SomeAST (..)
  , withSomeAST
  , astParseBlob
  , ASTFormat (..)
  , runASTParse
  ) where

import Control.Effect
import Control.Monad
import Data.AST
import Data.Blob
import Parsing.Parser
import Prologue
import Rendering.JSON (renderJSONAST)
import Semantic.Task
import qualified Serializing.Format as F

data SomeAST where
  SomeAST :: Show grammar => AST [] grammar -> SomeAST

withSomeAST :: (forall grammar . Show grammar => AST [] grammar -> a) -> SomeAST -> a
withSomeAST f (SomeAST ast) = f ast

astParseBlob :: (Member (Error SomeException) sig, Member Task sig, Carrier sig m, Functor m) => Blob -> m SomeAST
astParseBlob blob@Blob{..}
  | Just (SomeASTParser parser) <- someASTParser blobLanguage = SomeAST <$> parse parser blob
  | otherwise = noLanguageForBlob blobPath


data ASTFormat = SExpression | JSON | Show
  deriving (Show)

runASTParse :: (Member Distribute sig, Member (Error SomeException) sig, Member Task sig, Carrier sig m, Monad m) => ASTFormat -> [Blob] -> m F.Builder
runASTParse SExpression = distributeFoldMap (astParseBlob >=> withSomeAST (serialize (F.SExpression F.ByShow)))
runASTParse Show        = distributeFoldMap (astParseBlob >=> withSomeAST (serialize F.Show . fmap nodeSymbol))
runASTParse JSON        = distributeFoldMap (\ blob -> astParseBlob blob >>= withSomeAST (render (renderJSONAST blob))) >=> serialize F.JSON
