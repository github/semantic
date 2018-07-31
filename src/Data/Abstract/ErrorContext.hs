{-# LANGUAGE KindSignatures #-}

module Data.Abstract.ErrorContext where

import Data.Abstract.Module (ModuleInfo)
import Data.Span (Span)

data ErrorContext = ErrorContext { moduleInfo :: ModuleInfo , span :: Span } deriving (Show)

data BaseError (exc :: * -> *) resume = BaseError { errorContext :: ErrorContext, err :: exc resume } deriving (Show)
