{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Semantic.Task.Files
  ( Files
  , Destination (..)
  , Source (..)
  , runFiles
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
import           Data.Language
import           Prelude hiding (readFile)
import           Semantic.IO
import qualified System.IO as IO hiding (withBinaryFile)
import qualified System.Path as Path
import qualified System.Path.IO as IO (withBinaryFile)

data Source blob where
  FromPath       :: File Language                   -> Source Blob
  FromHandle     :: Handle 'IO.ReadMode             -> Source [Blob]
  FromDir        :: Path.AbsRelDir                  -> Source [Blob]
  FromPathPair   :: File Language -> File Language  -> Source BlobPair
  FromPairHandle :: Handle 'IO.ReadMode             -> Source [BlobPair]

data Destination = ToPath Path.AbsRelFile | ToHandle (Handle 'IO.WriteMode)

-- | An effect to read/write 'Blob's from 'Handle's or 'FilePath's.
data Files (m :: * -> *) k
  = forall a . Read (Source a)                                     (a -> m k)
  | ReadProject (Maybe Path.AbsRelDir) Path.AbsRelFileDir Language [Path.AbsRelDir] (Project -> m k)
  | FindFiles Path.AbsRelDir [String] [Path.AbsRelDir] ([Path.AbsRelFile] -> m k)
  | Write Destination B.Builder                               (m k)

deriving instance Functor m => Functor (Files m)

instance HFunctor Files where
  hmap f (Read s k)                = Read s (f . k)
  hmap f (ReadProject mp p l ps k) = ReadProject mp p l ps (f . k)
  hmap f (FindFiles p s ps k)      = FindFiles p s ps (f . k)
  hmap f (Write d b k)             = Write d b (f k)

instance Effect Files where
  thread state handler (Read s k)                = Read s (handler . (<$ state) . k)
  thread state handler (ReadProject mp p l ps k) = ReadProject mp p l ps (handler . (<$ state) . k)
  thread state handler (FindFiles p s ps k)      = FindFiles p s ps (handler . (<$ state) . k)
  thread state handler (Write d b k)             = Write d b (handler . (<$ state) $ k)

-- | Run a 'Files' effect in 'IO'
runFiles :: FilesC m a -> m a
runFiles = runFilesC

newtype FilesC m a = FilesC { runFilesC :: m a }
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO)

instance (Has (Error SomeException) sig m, MonadFail m, MonadIO m) => Algebra (Files :+: sig) (FilesC m) where
  alg (R other) = FilesC (alg (handleCoercible other))
  alg (L op) = case op of
    Read (FromPath path) k                                    -> readBlobFromFile' path >>= k
    Read (FromHandle handle) k                                -> readBlobsFromHandle handle >>= k
    Read (FromDir dir) k                                      -> readBlobsFromDir dir >>= k
    Read (FromPathPair p1 p2) k                               -> readFilePair p1 p2 >>= k
    Read (FromPairHandle handle) k                            -> readBlobPairsFromHandle handle >>= k
    ReadProject rootDir dir language excludeDirs k            -> readProjectFromPaths rootDir dir language excludeDirs >>= k
    FindFiles dir exts excludeDirs k                          -> findFilesInDir dir exts excludeDirs >>= k
    Write (ToPath path) builder k                             -> liftIO (IO.withBinaryFile path IO.WriteMode (`B.hPutBuilder` builder)) >> k
    Write (ToHandle (WriteHandle handle)) builder k           -> liftIO (B.hPutBuilder handle builder) >> k

readBlob :: Has Files sig m => File Language -> m Blob
readBlob file = send (Read (FromPath file) pure)

-- Various ways to read in files
data FilesArg
  = FilesFromHandle (Handle 'IO.ReadMode)
  | FilesFromPaths [File Language]

-- | A task which reads a list of 'Blob's from a 'Handle' or a list of 'FilePath's optionally paired with 'Language's.
readBlobs :: Has Files sig m => FilesArg -> m [Blob]
readBlobs (FilesFromHandle handle) = send (Read (FromHandle handle) pure)
readBlobs (FilesFromPaths paths)   = traverse (send . flip Read pure . FromPath) paths

-- | A task which reads a list of pairs of 'Blob's from a 'Handle' or a list of pairs of 'FilePath's optionally paired with 'Language's.
readBlobPairs :: Has Files sig m => Either (Handle 'IO.ReadMode) [(File Language, File Language)] -> m [BlobPair]
readBlobPairs (Left handle) = send (Read (FromPairHandle handle) pure)
readBlobPairs (Right paths) = traverse (send . flip Read pure . uncurry FromPathPair) paths

readProject :: Has Files sig m => Maybe Path.AbsRelDir -> Path.AbsRelFileDir -> Language -> [Path.AbsRelDir] -> m Project
readProject rootDir dir lang excludeDirs = send (ReadProject rootDir dir lang excludeDirs pure)

findFiles :: Has Files sig m => Path.AbsRelDir -> [String] -> [Path.AbsRelDir] -> m [Path.AbsRelFile]
findFiles dir exts paths = send (FindFiles dir exts paths pure)

-- | A task which writes a 'B.Builder' to a 'Handle' or a 'FilePath'.
write :: Has Files sig m => Destination -> B.Builder -> m ()
write dest builder = send (Write dest builder (pure ()))
