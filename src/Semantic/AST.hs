{-# LANGUAGE GADTs, RankNTypes #-}
module Semantic.AST
  ( SomeAST (..)
  , withSomeAST
  , astParseBlob
  , ASTFormat (..)
  , runASTParse
  ) where

import Data.AST
import Data.Blob
import Parsing.Parser
import Prologue hiding (MonadError(..))
import Rendering.JSON (renderJSONAST)
import Semantic.IO (noLanguageForBlob)
import Semantic.Task
import qualified Serializing.Format as F

data SomeAST where
  SomeAST :: Show grammar => AST [] grammar -> SomeAST

withSomeAST :: (forall grammar . Show grammar => AST [] grammar -> a) -> SomeAST -> a
withSomeAST f (SomeAST ast) = f ast

astParseBlob :: (Member (Exc SomeException) effs, Member Task effs) => Blob -> Eff effs SomeAST
astParseBlob blob@Blob{..}
  | Just (SomeASTParser parser) <- someASTParser blobLanguage = SomeAST <$> parse parser blob
  | otherwise = noLanguageForBlob blobPath


data ASTFormat = SExpression | JSON | Show
  deriving (Show)

runASTParse :: (Member Distribute effects, Member (Exc SomeException) effects, Member Task effects) => ASTFormat -> [Blob] -> Eff effects F.Builder
runASTParse SExpression = distributeFoldMap (astParseBlob >=> withSomeAST (serialize (F.SExpression F.ByShow)))
runASTParse Show        = distributeFoldMap (astParseBlob >=> withSomeAST (serialize F.Show))
runASTParse JSON        = distributeFoldMap (\ blob -> astParseBlob blob >>= withSomeAST (render (renderJSONAST blob))) >=> serialize F.JSON
