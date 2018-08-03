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
          path = "path: " <> show (M.modulePath moduleInfo)
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

instance Show1 (BaseError exc) where
  liftShowsPrec sp sl d = liftShowsPrec sp sl d

instance Lower (ErrorContext) where
  lowerBound = ErrorContext lowerBound lowerBound
