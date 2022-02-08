module Analysis.Exception
( Exception(..)
) where

newtype Exception = Exception { exceptionName :: String }
