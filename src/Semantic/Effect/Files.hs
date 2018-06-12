{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Semantic.Effect.Files
  ( Source (..)
  , Destination (..)
  , Files (..)
  , catchException
  , findFiles
  , readBlob
  , readBlobPairs
  , readBlobs
  , readProject
  , runFiles
  , write
  ) where

import qualified Control.Exception as Exc
import           Control.Monad.Effect
import           Control.Monad.Effect.Exception
import           Control.Monad.Effect.State
import           Control.Monad.IO.Class
import           Data.Blob
import qualified Data.ByteString.Builder as B
import           Data.Handle
import           Data.Language
import           Data.Project (File (..), ProjectException (..))
import qualified Data.Project as Project
import           Prelude hiding (readFile)
import           Prologue hiding (MonadError (..), fail)
import qualified System.IO as IO
import           Semantic.IO

data Source blob where
  FromPath       :: File                -> Source Blob
  FromHandle     :: Handle 'IO.ReadMode -> Source [Blob]
  FromPathPair   :: Both File           -> Source BlobPair
  FromPairHandle :: Handle 'IO.ReadMode -> Source [BlobPair]

data Destination = ToPath FilePath | ToHandle (Handle 'IO.WriteMode)

-- | An effect to read/write 'Blob's from 'Handle's or 'FilePath's.
data Files out where
  Read        :: Source out -> Files out
  ReadProject :: Maybe FilePath -> FilePath -> Language -> [FilePath] -> Files Project.Concrete
  FindFiles   :: FilePath -> [String] -> [FilePath] -> Files [FilePath]
  Write       :: Destination -> B.Builder -> Files ()

readBlob :: Member Files effs => File -> Eff effs Blob
readBlob = send . Read . FromPath

-- | A task which reads a list of 'Blob's from a 'Handle' or a list of 'FilePath's optionally paired with 'Language's.
readBlobs :: Member Files effs => Either (Handle 'IO.ReadMode) [File] -> Eff effs [Blob]
readBlobs (Left handle) = send (Read (FromHandle handle))
readBlobs (Right paths) = traverse (send . Read . FromPath) paths

-- | A task which reads a list of pairs of 'Blob's from a 'Handle' or a list of pairs of 'FilePath's optionally paired with 'Language's.
readBlobPairs :: Member Files effs => Either (Handle 'IO.ReadMode) [Both File] -> Eff effs [BlobPair]
readBlobPairs (Left handle) = send (Read (FromPairHandle handle))
readBlobPairs (Right paths) = traverse (send . Read . FromPathPair) paths

readProject :: Member Files effs => Maybe FilePath -> FilePath -> Language -> [FilePath] -> Eff effs Project.Concrete
readProject rootDir dir excludeDirs = send . ReadProject rootDir dir excludeDirs

findFiles :: Member Files effs => FilePath -> [String] -> [FilePath] -> Eff effs [FilePath]
findFiles dir exts = send . FindFiles dir exts

-- | A task which writes a 'B.Builder' to a 'Handle' or a 'FilePath'.
write :: Member Files effs => Destination -> B.Builder -> Eff effs ()
write dest = send . Write dest


-- | Run a 'Files' effect in 'IO'.
runFiles :: (Member (Exc SomeException) effs, Member IO effs) => Eff (Files ': effs) a -> Eff effs a
runFiles = interpret $ \ files -> case files of
  Read (FromPath path)         -> rethrowing (readBlobFromPath path)
  Read (FromHandle handle)     -> rethrowing (readBlobsFromHandle handle)
  Read (FromPathPair paths)    -> rethrowing (runBothWith readFilePair paths)
  Read (FromPairHandle handle) -> rethrowing (readBlobPairsFromHandle handle)
  ReadProject rootDir dir language excludeDirs -> rethrowing (readProjectFromPaths rootDir dir language excludeDirs)
  FindFiles dir exts excludeDirs -> rethrowing (findFilesInDir dir exts excludeDirs)
  Write (ToPath path)                   builder -> liftIO (IO.withBinaryFile path IO.WriteMode (`B.hPutBuilder` builder))
  Write (ToHandle (WriteHandle handle)) builder -> liftIO (B.hPutBuilder handle builder)

-- | Catch exceptions in 'IO' actions embedded in 'Eff', handling them with the passed function.
--
--   Note that while the type allows 'IO' to occur anywhere within the effect list, it must actually occur at the end to be able to run the computation.
catchException :: ( Exc.Exception e
                  , Member IO r
                  )
               => Eff r a
               -> (e -> Eff r a)
               -> Eff r a
catchException m handler = interpose pure (\ m yield -> send (Exc.try m) >>= either handler yield) m

-- | Lift an 'IO' action into 'Eff', catching and rethrowing any exceptions it throws into an 'Exc' effect.
rethrowing :: ( Member (Exc SomeException) r
              , Member IO r
              )
           => IO a
           -> Eff r a
rethrowing m = catchException (liftIO m) (throwError . toException @SomeException)
