{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Semantic.Task.Files
  ( Files
  , Destination (..)
  , Source (..)
  , readBlob
  , readBlobs
  , readBlobPairs
  , readProject
  , findFiles
  , write
  , Handle (..)
  , FilesC(..)
  , FilesArg(..)
  ) where

import           Analysis.File
import           Analysis.Project
import           Control.Algebra
import           Control.Effect.Error
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Blob
import           Data.Blob.IO
import qualified Data.ByteString.Builder as B
import           Data.Handle
import           Prelude hiding (readFile)
import           Semantic.IO
import           Source.Language (Language)
import qualified System.IO as IO

data Source blob where
  FromPath       :: File Language                   -> Source Blob
  FromHandle     :: Handle 'IO.ReadMode             -> Source [Blob]
  FromPathPair   :: File Language -> File Language  -> Source BlobPair
  FromPairHandle :: Handle 'IO.ReadMode             -> Source [BlobPair]

data Destination = ToPath FilePath | ToHandle (Handle 'IO.WriteMode)

-- | An effect to read/write 'Blob's from 'Handle's or 'FilePath's.
data Files (m :: * -> *) k where
  Read :: Source a -> Files m a
  ReadProject :: Maybe FilePath -> FilePath -> Language -> [FilePath] -> Files m Project
  FindFiles :: FilePath -> [String] -> [FilePath] -> Files m [FilePath]
  Write :: Destination -> B.Builder -> Files m ()


newtype FilesC m a = FilesC
  { -- | Run a 'Files' effect in 'IO'
    runFiles :: m a
  }
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO)

instance (Has (Error SomeException) sig m, MonadFail m, MonadIO m) => Algebra (Files :+: sig) (FilesC m) where
  alg hdl sig ctx = case sig of
    L op -> (<$ ctx) <$> case op of
      Read (FromPath path)                                    -> readBlobFromFile' path
      Read (FromHandle handle)                                -> readBlobsFromHandle handle
      Read (FromPathPair p1 p2)                               -> readFilePair p1 p2
      Read (FromPairHandle handle)                            -> readBlobPairsFromHandle handle
      ReadProject rootDir dir language excludeDirs            -> readProjectFromPaths rootDir dir language excludeDirs
      FindFiles dir exts excludeDirs                          -> findFilesInDir dir exts excludeDirs
      Write (ToPath path) builder                             -> liftIO (IO.withBinaryFile path IO.WriteMode (`B.hPutBuilder` builder))
      Write (ToHandle (WriteHandle handle)) builder           -> liftIO (B.hPutBuilder handle builder)
    R other -> FilesC (alg (runFiles . hdl) other ctx)

readBlob :: Has Files sig m => File Language -> m Blob
readBlob file = send (Read (FromPath file))

-- Various ways to read in files
data FilesArg
  = FilesFromHandle (Handle 'IO.ReadMode)
  | FilesFromPaths [File Language]

-- | A task which reads a list of 'Blob's from a 'Handle' or a list of 'FilePath's optionally paired with 'Language's.
readBlobs :: Has Files sig m => FilesArg -> m [Blob]
readBlobs (FilesFromHandle handle) = send (Read (FromHandle handle))
readBlobs (FilesFromPaths paths)   = traverse (send . Read . FromPath) paths

-- | A task which reads a list of pairs of 'Blob's from a 'Handle' or a list of pairs of 'FilePath's optionally paired with 'Language's.
readBlobPairs :: Has Files sig m => Either (Handle 'IO.ReadMode) [(File Language, File Language)] -> m [BlobPair]
readBlobPairs (Left handle) = send (Read (FromPairHandle handle))
readBlobPairs (Right paths) = traverse (send . Read . uncurry FromPathPair) paths

readProject :: Has Files sig m => Maybe FilePath -> FilePath -> Language -> [FilePath] -> m Project
readProject rootDir dir lang excludeDirs = send (ReadProject rootDir dir lang excludeDirs)

findFiles :: Has Files sig m => FilePath -> [String] -> [FilePath] -> m [FilePath]
findFiles dir exts paths = send (FindFiles dir exts paths)

-- | A task which writes a 'B.Builder' to a 'Handle' or a 'FilePath'.
write :: Has Files sig m => Destination -> B.Builder -> m ()
write dest builder = send (Write dest builder)
