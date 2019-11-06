{-# LANGUAGE FlexibleContexts, KindSignatures, RecordWildCards #-}
module Data.Abstract.BaseError (
  BaseError(..)
, throwBaseError
)
where

import Control.Abstract.Context
import Control.Abstract.Evaluator
import qualified Data.Abstract.Module as M
import Prologue
import qualified Source.Span as S

data BaseError (exc :: * -> *) resume = BaseError { baseErrorModuleInfo :: ModuleInfo, baseErrorSpan :: Span, baseErrorException :: exc resume }

instance (Show (exc resume)) => Show (BaseError exc resume) where
  showsPrec _ BaseError{..} = shows baseErrorException <> showString " " <> showString errorLocation
    where errorLocation | startErrorLine == endErrorLine = M.modulePath baseErrorModuleInfo <> " " <> startErrorLine <> ":" <> startErrorCol <> "-" <> endErrorCol
                        | otherwise = M.modulePath baseErrorModuleInfo <> " " <> startErrorLine <> ":" <> startErrorCol <> "-" <> endErrorLine <> ":" <> endErrorCol
          startErrorLine = show $ S.line   (S.start baseErrorSpan)
          endErrorLine   = show $ S.line   (S.end   baseErrorSpan)
          startErrorCol  = show $ S.column (S.start baseErrorSpan)
          endErrorCol    = show $ S.column (S.end   baseErrorSpan)

instance (Eq1 exc) => Eq1 (BaseError exc) where
  liftEq f (BaseError info1 span1 exc1) (BaseError info2 span2 exc2) = info1 == info2 && span1 == span2 && liftEq f exc1 exc2

instance Show1 exc => Show1 (BaseError exc) where
  liftShowsPrec sl sp d (BaseError info span exc) = showParen (d > 10) $ showString "BaseError" . showChar ' ' . showsPrec 11 info . showChar ' ' . showsPrec 11 span . showChar ' ' . liftShowsPrec sl sp 11 exc

throwBaseError :: ( Member (Resumable (BaseError exc)) sig
                  , Member (Reader M.ModuleInfo) sig
                  , Member (Reader S.Span) sig
                  , Carrier sig m
                  )
                => exc resume
                -> m resume
throwBaseError err = do
  moduleInfo <- currentModule
  span <- currentSpan
  throwResumable $ BaseError moduleInfo span err
