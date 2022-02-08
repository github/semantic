module Analysis.Exception
( Exception(..)
) where

newtype Exception = Exception { exceptionName :: String }
  deriving (Eq, Ord, Show)
