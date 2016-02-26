module Control.Monad.Free where

data Free functor pure = Free (functor (Free functor pure)) | Pure pure
