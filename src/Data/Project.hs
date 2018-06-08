{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}

module Data.Project where

import Prelude hiding (readFile)
import Prologue hiding (throwError)

import Control.Monad.Effect
import Control.Monad.IO.Class
import Data.Source
import Control.Monad.Effect.Exception
import Data.Blob
import Data.Language
import qualified Data.Text as T
import Proto3.Suite
import System.FilePath.Posix
import qualified Data.ByteString as B
import Debug.Trace

data Project blobs paths path = Project
  { projectRootDir     :: path
  , projectBlobs       :: blobs Blob
  , projectLanguage    :: Language
  , projectEntryPaths  :: paths path
  , projectExcludeDirs :: paths path
  } deriving (Functor, Generic, Named)

deriving instance ( MessageField path
                  , MessageField (paths path)
                  , MessageField (blobs Blob)
                  ) => Message (Project blobs paths path)

deriving instance (Eq path, Eq (blobs Blob), Eq (paths path)) => Eq (Project blobs paths path)
deriving instance (Show path, Show (blobs Blob), Show (paths path)) => Show (Project blobs paths path)

type Concrete = Project [] [] FilePath
type PB = Project NestedVec UnpackedVec Text

fromPB :: PB -> Concrete
fromPB Project {..} = Project
  { projectRootDir     = T.unpack projectRootDir
  , projectBlobs       = go projectBlobs
  , projectLanguage    = projectLanguage
  , projectEntryPaths  = T.unpack <$> go projectEntryPaths
  , projectExcludeDirs = T.unpack <$> go projectExcludeDirs
  } where go :: Foldable f => f a -> [a]
          go = foldr (:) []

projectName :: Concrete -> Text
projectName = T.pack . dropExtensions . takeFileName . projectRootDir

projectExtensions :: Concrete -> [String]
projectExtensions = extensionsForLanguage . projectLanguage

projectEntryPoints :: Concrete -> [File]
projectEntryPoints (Project {..})= foldr go [] projectBlobs
  where go b acc =
          if blobPath b `elem` projectEntryPaths
          then toFile b : acc
          else acc

projectFiles :: Concrete -> [File]
projectFiles = fmap toFile . projectBlobs where


data File = File
  { filePath     :: FilePath
  , fileLanguage :: Language
  } deriving (Eq, Ord, Show)

file :: FilePath -> File
file path = File path (languageForFilePath path)
  where languageForFilePath = languageForType . takeExtension

toFile :: Blob -> File
toFile (Blob _ p l) = File p l

data ProjectException
  = FileNotFound FilePath
  | EmptyPairProvided
  | PairNotFound (Both FilePath)
  | HandleNotSupported
  | WritesNotSupported
  | NoLanguagesSpecified
  | UnknownLanguage
  | MultipleLanguagesSpecified [Language]
  | TODO
    deriving (Show, Eq, Typeable, Exception)

readBlobFromPath :: Member (Exc SomeException) effs
                 => Concrete
                 -> File
                 -> Eff effs Blob
readBlobFromPath g f = readFile g f >>= maybeM (throwError (SomeException (FileNotFound (filePath f))))

addPrelude :: MonadIO m
           => Concrete
           -> File
           -> m Concrete
addPrelude g File{..} = do
  traceM "Adding to prelude"
  contents <- liftIO (B.readFile filePath)
  let blob = Blob (fromUTF8 contents) filePath fileLanguage
  pure $ g { projectBlobs = blob : projectBlobs g }

readFile :: Member (Exc SomeException) effs
         => Concrete
         -> File
         -> Eff effs (Maybe Blob)
readFile Project{..} f =
  let p         = filePath f
      candidate = find (\b -> blobPath b == p) (traceShowId projectBlobs)
  in if
    | p == "/dev/null"  -> pure Nothing
    | isJust candidate  -> pure candidate
    | otherwise         -> throwError (SomeException (FileNotFound p))

readBlobPair :: Member (Exc SomeException) effs
             => Concrete
             -> File
             -> File
             -> Eff effs BlobPair
readBlobPair g f1 f2 = Join <$> join (maybeThese <$> readFile g f1 <*> readFile g f2)

maybeThese :: Member (Exc SomeException) effs => Maybe a -> Maybe b -> Eff effs (These a b)
maybeThese a b = case (a, b) of
  (Just a, Nothing) -> pure (This a)
  (Nothing, Just b) -> pure (That b)
  (Just a, Just b)  -> pure (These a b)
  _                 -> throwError (SomeException EmptyPairProvided)

findFiles :: Member (Exc SomeException) effs
          => Concrete
          -> FilePath
          -> [String]
          -> Eff effs [FilePath]
findFiles _ _ _ = throwError (SomeException TODO)
