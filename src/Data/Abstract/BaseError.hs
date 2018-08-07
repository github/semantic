{-# LANGUAGE KindSignatures, AllowAmbiguousTypes #-}

module Data.Abstract.BaseError (
  BaseError(..)
, throwBaseError
)
where

import Control.Abstract.Context
import Control.Abstract.Evaluator
import qualified Data.Abstract.Module as M
import qualified Data.Span as S
import Prologue

data BaseError (exc :: * -> *) resume = BaseError { baseErrorModuleInfo :: ModuleInfo, baseErrorSpan :: Span, baseErrorException :: exc resume }

instance (Show (exc resume)) => Show (BaseError exc resume) where
  showsPrec _ BaseError{..} = shows baseErrorException <> showString " " <> showString errorLocation
    where errorLocation | startErrorLine == endErrorLine = M.modulePath baseErrorModuleInfo <> " " <> startErrorLine <> ":" <> startErrorCol <> "-" <> endErrorCol
                        | otherwise = M.modulePath baseErrorModuleInfo <> " " <> startErrorLine <> ":" <> startErrorCol <> "-" <> endErrorLine <> ":" <> endErrorCol
          startErrorLine = show $ S.posLine (S.spanStart baseErrorSpan)
          endErrorLine   = show $ S.posLine (S.spanEnd baseErrorSpan)
          startErrorCol  = show $ S.posColumn (S.spanStart baseErrorSpan)
          endErrorCol    = show $ S.posColumn (S.spanEnd baseErrorSpan)

instance (Eq1 exc) => Eq1 (BaseError exc) where
  liftEq f (BaseError _ _ exc1) (BaseError _ _ exc2) = liftEq f exc1 exc2

instance Show1 exc => Show1 (BaseError exc) where
  liftShowsPrec sl sp d (BaseError _ _ exc) = liftShowsPrec sl sp d exc

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
