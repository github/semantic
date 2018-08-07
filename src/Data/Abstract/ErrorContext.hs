{-# LANGUAGE KindSignatures #-}

module Data.Abstract.ErrorContext (
  BaseError(..)
, ErrorContext(..)
)
where

import qualified Data.Abstract.Module as M
import qualified Data.Span as S
import Prologue

data BaseError (exc :: * -> *) resume = BaseError { baseErrorModuleInfo :: ModuleInfo, baseErrorSpan :: Span, baseErrorException :: exc resume }


instance (Eq1 exc) => Eq1 (BaseError exc) where
  liftEq f (BaseError _ exc1) (BaseError _ exc2) = liftEq f exc1 exc2

instance Show1 exc => Show1 (BaseError exc) where
  liftShowsPrec sl sp d (BaseError _ exc) = liftShowsPrec sl sp d exc

throwBaseError :: ( Member (Resumable (BaseError exc)) effects
                  , Member (Reader M.ModuleInfo) effects
                  , Member (Reader S.Span) effects
                  )
                => exc resume
                -> Evaluator address value effects resume
throwBaseError err = do
  moduleInfo <- currentModule
  span <- currentSpan
  throwResumable $ BaseError moduleInfo span err
