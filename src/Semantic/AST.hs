{-# LANGUAGE GADTs, RankNTypes #-}
module Semantic.AST where

import Data.AST
import Data.Blob
import Parsing.Parser
import Prologue hiding (MonadError(..))
import Rendering.JSON
import Semantic.IO (noLanguageForBlob)
import Semantic.Task
import qualified Serializing.Format as F

data SomeAST where
  SomeAST :: Show grammar => AST [] grammar -> SomeAST

withSomeAST :: (forall grammar . Show grammar => AST [] grammar -> a) -> SomeAST -> a
withSomeAST f (SomeAST ast) = f ast

astParseBlob :: Members '[Task, Exc SomeException] effs => Blob -> Eff effs SomeAST
astParseBlob blob@Blob{..}
  | Just (SomeASTParser parser) <- someASTParser <$> blobLanguage
  = SomeAST <$> parse parser blob
  | otherwise = noLanguageForBlob blobPath


data ASTFormat = SExpression | JSON
  deriving (Show)

runASTParse :: Members '[Distribute WrappedTask, Task, Exc SomeException] effects => ASTFormat -> [Blob] -> Eff effects F.Builder
runASTParse SExpression = distributeFoldMap (WrapTask . (withSomeAST (serialize (F.SExpression F.ByShow)) <=< astParseBlob))
runASTParse JSON        = serialize F.JSON <=< distributeFoldMap (\ blob -> WrapTask (withSomeAST (render (renderJSONAST blob)) =<< astParseBlob blob))
