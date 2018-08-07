{-# LANGUAGE KindSignatures #-}

module Data.Abstract.ErrorContext (
  BaseError(..)
, ErrorContext(..)
)
where

import qualified Data.Abstract.Module as M
import qualified Data.Span as S
import Prologue

data BaseError (exc :: * -> *) resume = BaseError { errorContext :: ErrorContext, err :: exc resume } deriving (Show)

data ErrorContext = ErrorContext { moduleInfo :: M.ModuleInfo , span :: S.Span }

instance Show ErrorContext where
  showsPrec _ ErrorContext{..} = shows errorString
    where errorString = path <> position
          path = show (M.modulePath moduleInfo)
          position | startLine <- S.posLine (S.spanStart span)
                   , endLine <- S.posLine (S.spanEnd span)
                   , startLine == endLine
                   = let startLine = show (S.posLine (S.spanStart span))
                         startCol  = show (S.posColumn (S.spanStart span))
                         endCol    = show (S.posColumn (S.spanEnd span))
                         in startLine <> ":" <> startCol <> "-" <> endCol
                   | otherwise = let startLine = show (S.posLine (S.spanStart span))
                                     endLine   = show (S.posLine (S.spanEnd span))
                                     startCol  = show (S.posColumn (S.spanStart span))
                                     endCol    = show (S.posColumn (S.spanEnd span))
                                     in startLine <> ":" <> startCol <> "-" <> endLine <> ":" <> endCol

instance (Eq1 exc) => Eq1 (BaseError exc) where
  liftEq f (BaseError _ exc1) (BaseError _ exc2) = liftEq f exc1 exc2

instance Show1 exc => Show1 (BaseError exc) where
  liftShowsPrec sl sp d (BaseError _ exc) = liftShowsPrec sl sp d exc
