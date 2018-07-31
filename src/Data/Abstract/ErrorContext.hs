{-# LANGUAGE KindSignatures #-}

module Data.Abstract.ErrorContext (
  BaseError(..)
, ErrorContext(..)
)
where

data ErrorContext = ErrorContext { moduleInfo :: ModuleInfo , span :: Span } deriving (Show)
import qualified Data.Abstract.Module as M
import qualified Data.Span as S

data BaseError (exc :: * -> *) resume = BaseError { errorContext :: ErrorContext, err :: exc resume } deriving (Show)
