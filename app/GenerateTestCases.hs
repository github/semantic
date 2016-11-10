{-# LANGUAGE LambdaCase, GADTs, DataKinds #-}
module Main where

import Arguments
import Control.Exception
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as DL
import qualified Data.ByteString.Char8 as DC
import Data.String
import qualified Data.Text as DT
import JSONTestCase
import qualified Prelude
import Prologue
import SemanticDiff (fetchDiffs)
import System.FilePath.Glob
import System.Process
import qualified Data.String.Utils as DSUtils
import Options.Applicative hiding ((<>))
import qualified Options.Applicative as O
import qualified Renderer as R
import Control.Monad.Effect
import Control.Monad.Effect.Internal

data GenerateFormat =
    GenerateSummaries
  | GenerateJSON
  deriving (Show)

data GeneratorArgs = GeneratorArgs { generateFormat :: GenerateFormat } deriving (Show)

generatorArgs :: Parser GeneratorArgs
generatorArgs = GeneratorArgs
  <$> (flag' GenerateSummaries (long "generate-summaries" O.<> short 's' O.<> help "Generates summary results for new JSON test cases")
  <|> flag' GenerateJSON (long "generate-json" O.<> short 'j' O.<> help "Generate JSON output for new JSON test cases"))

options :: ParserInfo GeneratorArgs
options = info (helper <*> generatorArgs) (fullDesc O.<> progDesc "Auto-generate JSON test cases" O.<> header "JSON Test Case Generator")

main :: IO ()
main = do
  opts <- execParser options
  generatorFilePaths <- runFetchGeneratorFiles
  metaRepos <- traverse DL.readFile generatorFilePaths

  for_ (decodeMetaRepos metaRepos) (handle opts generatorFilePaths)

  where decodeMetaRepos :: [DL.ByteString] -> [Either String [JSONMetaRepo]]
        decodeMetaRepos metaRepos = eitherDecode <$> metaRepos

        handle :: GeneratorArgs -> [FilePath] -> Either String [JSONMetaRepo] -> IO ()
        handle opts generatorFilePaths decodedMetaRepos =
          case decodedMetaRepos of
            Left err ->  Prelude.putStrLn $ "An error occurred: " <> err
            Right metaRepos -> do
              traverse_ (runGenerator opts) metaRepos
              traverse_ runMoveGeneratorFile generatorFilePaths

-- | Finds all JSON files within the generators directory.
runFetchGeneratorFiles :: IO [FilePath]
runFetchGeneratorFiles = globDir1 (compile "*.json") "test/corpus/generators"

-- | First initialize the git submodule repository where commits will be made for the given metaRepo and its syntaxes.
-- | Second generate the commits for each syntax and generate the associated JSONTestCase objects.
-- | Finally push the generated commits to the submodule's remote repository.
runGenerator :: GeneratorArgs -> JSONMetaRepo -> IO ()
runGenerator opts metaRepo@JSONMetaRepo{..} = do
  runSetupGitRepo repoUrl $ repoPath language
  runCommitsAndTestCasesGeneration opts metaRepo
  runPullGitRemote repoUrl $ repoPath language
  runPushGitRemote $ repoPath language

-- | Defines the repoPath based on the convention that a repository is based on its language name for a defaut location.
repoPath :: String -> FilePath
repoPath language = "test/corpus/repos/" <> language

-- | Upon successful test case generation for a generator file, move the file to the generated directory.
-- | This prevents subsequence runs of the test generator from duplicating test cases and adding extraneous
-- | commits to the git submodule.
runMoveGeneratorFile :: FilePath -> IO ()
runMoveGeneratorFile filePath = do
  let updatedPath = DT.unpack $ DT.replace (DT.pack "generators") (DT.pack "generated") (DT.pack filePath)
  Prelude.putStrLn updatedPath
  _ <- readCreateProcess (shell $ "mv " <> filePath <> " " <> updatedPath) ""
  return ()

-- | Initializes a new git repository and adds it as a submodule to the semantic-diff git index.
-- | This repository contains the commits associated with the given JSONMetaRepo's syntax examples.
runSetupGitRepo :: String -> FilePath -> IO ()
runSetupGitRepo repoUrl repoPath = do
  runInitializeRepo repoUrl repoPath
  runAddSubmodule repoUrl repoPath

-- | Performs the system calls for initializing the git repository.
-- | If the git repository already exists, the operation will result in an error,
-- | but will not prevent successful completion of the test case generation.
runInitializeRepo :: String -> FilePath -> IO ()
runInitializeRepo repoUrl repoPath = do
  result <- try $ readCreateProcess (shell $ mkDirCommand repoPath) ""
  case (result :: Either Prelude.IOError String) of
    Left error -> Prelude.putStrLn $ "Creating the repository directory at " <> repoPath <> " failed with: " <> show error <> ". " <> "Possible reason: repository already initialized. \nProceeding to the next step."
    Right _ -> do
      _ <- executeCommand repoPath (initializeRepoCommand repoUrl)
      Prelude.putStrLn $ "Repository directory successfully initialized for " <> repoPath <> "."

-- | Git repositories generated as a side-effect of generating tests cases are
-- | added to semantic-diff's git index as submodules. If the submodule initialization
-- | fails (usually because the submodule was already initialized), operations will
-- | continue.
runAddSubmodule :: String -> FilePath -> IO ()
runAddSubmodule repoUrl repoPath = do
  result <- try $ readCreateProcess (shell $ addSubmoduleCommand repoUrl repoPath) ""
  case (result :: Either Prelude.IOError String) of
    Left error -> Prelude.putStrLn $ "Initializing the submodule repository at " <> repoPath <> " failed with: " <> show error <> ". " <> "Possible reason: submodule already initialized. \nProceeding to the next step."
    _ -> Prelude.putStrLn $ "Submodule successfully initialized for " <> repoPath <> "."

-- | Performs the system calls for generating the commits and test cases.
-- | Also appends the JSONTestCases generated to the test case file defined by
-- | the syntaxes.
runCommitsAndTestCasesGeneration :: GeneratorArgs -> JSONMetaRepo -> IO ()
runCommitsAndTestCasesGeneration opts metaRepo@JSONMetaRepo{..} =
  for_ syntaxes generate
    where
      generate :: JSONMetaSyntax -> IO ()
      generate metaSyntax = do
        _ <- runInitialCommitForSyntax metaRepo metaSyntax
        let testCaseFilePath' = testCaseFilePath language opts metaSyntax
        runSetupTestCaseFile testCaseFilePath'
        runCommitAndTestCaseGeneration opts metaRepo metaSyntax testCaseFilePath'
        runCloseTestCaseFile testCaseFilePath'

      testCaseFilePath :: String -> GeneratorArgs -> JSONMetaSyntax -> FilePath
      testCaseFilePath language GeneratorArgs{..} JSONMetaSyntax{..} = case generateFormat of
        GenerateSummaries -> "test/corpus/diff-summaries/" <> language <> "/" <> syntax <> ".json"
        GenerateJSON -> "test/corpus/json/" <> language <> "/" <> syntax <> ".json"

-- | For a syntax, we want the initial commit to be an empty file.
-- | This function performs a touch and commits the empty file.
runInitialCommitForSyntax :: JSONMetaRepo -> JSONMetaSyntax -> IO ()
runInitialCommitForSyntax metaRepo@JSONMetaRepo{..} metaSyntax@JSONMetaSyntax{..} = do
  Prelude.putStrLn $ "Generating initial commit for " <> syntax <> " syntax."

  let repoFilePath' = repoFilePath metaRepo metaSyntax

  result <- try . executeCommand (repoPath language) $ touchCommand repoFilePath' <> commitCommand syntax "Initial commit"
  case ( result :: Either Prelude.IOError String) of
    Left error -> Prelude.putStrLn $ "Initializing the " <> repoFilePath metaRepo metaSyntax <> " failed with: " <> show error <> ". " <> "Possible reason: file already initialized. \nProceeding to the next step."
    Right _ -> runAddTemplateForSyntax metaRepo metaSyntax

runAddTemplateForSyntax :: JSONMetaRepo -> JSONMetaSyntax -> IO ()
runAddTemplateForSyntax metaRepo@JSONMetaRepo{..} metaSyntax@JSONMetaSyntax{..} = case templateText of
  Just templateText -> do
    let repoFilePath' = repoFilePath metaRepo metaSyntax
    _ <- executeCommand (repoPath language) $ fileWriteCommand repoFilePath' templateText <> commitCommand syntax ("Add " <> repoFilePath' <> " template text.")
    pure ()
  Nothing -> pure ()

-- | Initializes the test case file where JSONTestCase examples are written to.
-- | This manually inserts a "[" to open a JSON array.
runSetupTestCaseFile :: FilePath -> IO ()
runSetupTestCaseFile testCaseFilePath = do
  Prelude.putStrLn $ "Opening " <> testCaseFilePath
  DL.writeFile testCaseFilePath "["

-- | For each command constructed for a given metaSyntax, execute the system commands.
runCommitAndTestCaseGeneration :: GeneratorArgs -> JSONMetaRepo -> JSONMetaSyntax -> FilePath -> IO ()
runCommitAndTestCaseGeneration opts metaRepo metaSyntax testCaseFilePath =
   traverse_ (runGenerateCommitAndTestCase opts metaRepo testCaseFilePath) (commands metaRepo metaSyntax)

-- | Converts a list of Output to a list of Renderer.Summary Map values
maybeMapSummary :: [R.Output] -> [Maybe (Map Text (Map Text [Value]))]
maybeMapSummary = fmap $ \case
    R.SummaryOutput output -> Just output
    _ -> Nothing

-- | Converst a list of Output to a list of Renderer.JSON values
maybeMapJSON :: [R.Output] -> [Maybe (Map Text Value)]
maybeMapJSON = fmap $ \case
  R.JSONOutput output -> Just output
  _ -> Nothing

-- | This function represents the heart of the test case generation. It keeps track of
-- | the git shas prior to running a command, fetches the git sha after a command, so that
-- | JSONTestCase objects can be created. Finally, it appends the created JSONTestCase
-- | object to the test case file.
runGenerateCommitAndTestCase :: GeneratorArgs -> JSONMetaRepo -> FilePath -> (JSONMetaSyntax, String, String, String) -> IO ()
runGenerateCommitAndTestCase opts JSONMetaRepo{..} testCaseFilePath (JSONMetaSyntax{..}, description, seperator, command) = do
  Prelude.putStrLn $ "Executing " <> syntax <> " " <> description <> " commit."

  beforeSha <- executeCommand (repoPath language) getLastCommitShaCommand
  _ <- executeCommand (repoPath language) command
  afterSha <- executeCommand (repoPath language) getLastCommitShaCommand

  patch <- executeCommand (repoPath language) (gitDiffCommand beforeSha afterSha)

  expectedResult' <- runExpectedResult (repoPath language) beforeSha afterSha (syntax <> fileExt) opts

  let jsonTestCase = encodePretty JSONTestCase {
    gitDir = extractGitDir (repoPath language),
    testCaseDescription = language <> "-" <> syntax <> "-" <> description <> "-" <> "test",
    filePaths = [syntax <> fileExt],
    shas = beforeSha <> ".." <> afterSha,
    patch = lines patch,
    expectedResult = expectedResult'
    }

  Prelude.putStrLn $ "Generating test case for " <> language <> ": " <> syntax <> " " <> description <> "."

  DL.appendFile testCaseFilePath $ jsonTestCase <> DL.fromStrict (DC.pack seperator)
  where extractGitDir :: String -> String
        extractGitDir fullRepoPath = DC.unpack $ snd $ DC.breakSubstring (DC.pack "test") (DC.pack fullRepoPath)

-- | This constructs an Eff and runs it to return the appropriate IO ExpectedResult.
runExpectedResult :: FilePath -> String -> String -> FilePath -> GeneratorArgs -> IO ExpectedResult
runExpectedResult repoPath beforeSha afterSha repoFilePath GeneratorArgs{..} =
  case generateFormat of
    GenerateSummaries -> Main.run $ constructSummariesEff repoPath beforeSha afterSha repoFilePath
    GenerateJSON -> Main.run $ constructJSONEff repoPath beforeSha afterSha repoFilePath

data GenerateEff a where
  GenerateSummaries' :: Arguments -> GenerateEff ExpectedResult
  GenerateJSON' :: Arguments -> GenerateEff ExpectedResult

-- | Construct an Eff whose queue includes only GenerateEff effects.
constructSummariesEff :: FilePath -> String -> String -> FilePath -> Eff '[GenerateEff] ExpectedResult
constructSummariesEff repoPath beforeSha afterSha repoFilePath = send $ GenerateSummaries' (args repoPath beforeSha afterSha [repoFilePath] R.Summary)

-- | Construct an Eff whose queue includes only GenerateEff effects.
constructJSONEff :: FilePath -> String -> String -> FilePath -> Eff '[GenerateEff] ExpectedResult
constructJSONEff repoPath beforeSha afterSha repoFilePath = send $ GenerateJSON' (args repoPath beforeSha afterSha [repoFilePath] R.JSON)

-- | Evaluate the Effs and return the IO ExpectedResult.
run :: Eff '[GenerateEff] ExpectedResult -> IO ExpectedResult
run (Val x) = pure x
run (E u queue) = case decompose u of
  (Right (GenerateSummaries' args)) -> generateSummaries args >>= \s -> Main.run (apply queue s)
  (Right (GenerateJSON' args)) -> generateJSON args >>= \s -> Main.run (apply queue s)
  (Left _) -> pure $ SummaryResult ( Map.fromList [ ("changes", Map.singleton mempty mempty), ("errors", Map.singleton mempty mempty) ] )

-- | Produces DiffSummary results for the given Arguments.
generateSummaries :: Arguments -> IO ExpectedResult
generateSummaries args@Arguments{..} = do
  diffs <- fetchDiffs args
  let headResult = Prelude.head $ maybeMapSummary diffs
  let changes = fromMaybe (fromList [("changes", mempty)]) headResult ! "changes"
  let errors = fromMaybe (fromList [("errors", mempty)]) headResult ! "errors"
  pure $ SummaryResult ( Map.fromList [ ("changes", changes), ("errors", errors) ] )

-- | Produces JSON output for the given Arguments.
generateJSON :: Arguments -> IO ExpectedResult
generateJSON args = do
  diffs <- fetchDiffs args
  let headResult = Prelude.head $ maybeMapJSON diffs
  let oids = fromMaybe (fromList [("oids", "")]) headResult ! "oids"
  let paths = fromMaybe (fromList [("output", "")]) headResult ! "paths"
  let rows = fromMaybe (fromList [("rows", "")]) headResult ! "rows"
  pure $ JSONResult ( Map.fromList [ ("oids", oids), ("paths", paths), ("rows", rows) ] )


repoFilePath :: JSONMetaRepo -> JSONMetaSyntax -> String
repoFilePath metaRepo metaSyntax = syntax metaSyntax <> fileExt metaRepo

-- | Commands represent the various combination of patches (insert, delete, replacement)
-- | for a given syntax.
commands :: JSONMetaRepo -> JSONMetaSyntax -> [(JSONMetaSyntax, String, String, String)]
commands JSONMetaRepo{..} metaSyntax@JSONMetaSyntax{..} = case template of
  (Just _) -> [ (metaSyntax, "setup", commaSeperator, fileWriteCommand repoFilePath (withTemplate "") <> commitCommand syntax "setup")
              , (metaSyntax, "insert", commaSeperator, fileWriteCommand repoFilePath (withTemplate insert) <> commitCommand syntax "insert")
              , (metaSyntax, "replacement", commaSeperator, fileWriteCommand repoFilePath (withTemplate replacement) <> commitCommand syntax "replacement")
              , (metaSyntax, "delete-replacement", commaSeperator, fileWriteCommand repoFilePath (withTemplate insert) <> commitCommand syntax "delete replacement")
              , (metaSyntax, "delete-insert", commaSeperator, fileWriteCommand repoFilePath (withTemplate "") <> commitCommand syntax "delete insert")
              , (metaSyntax, "teardown", spaceSeperator, removeCommand repoFilePath <> touchCommand repoFilePath <> commitCommand syntax "teardown")
              ]
  Nothing -> [ (metaSyntax, "insert", commaSeperator, fileWriteCommand repoFilePath insert <> commitCommand syntax "insert")
             , (metaSyntax, "replacement-insert", commaSeperator, fileWriteCommand repoFilePath (Prologue.intercalate "\n" [replacement, insert, insert]) <> commitCommand syntax "replacement + insert + insert")
             , (metaSyntax, "delete-insert", commaSeperator, fileWriteCommand repoFilePath (Prologue.intercalate "\n" [insert, insert, insert]) <> commitCommand syntax "delete + insert")
             , (metaSyntax, "replacement", commaSeperator, fileWriteCommand repoFilePath (Prologue.intercalate "\n" [replacement, insert, insert]) <> commitCommand syntax "replacement")
             , (metaSyntax, "delete-replacement", commaSeperator, fileWriteCommand repoFilePath (Prologue.intercalate "\n" [insert, replacement]) <> commitCommand syntax "delete + replacement")
             , (metaSyntax, "delete", commaSeperator, fileWriteCommand repoFilePath replacement <> commitCommand syntax "delete")
             , (metaSyntax, "delete-rest", spaceSeperator, removeCommand repoFilePath <> touchCommand repoFilePath <> commitCommand syntax "delete rest")
             ]
  where commaSeperator = "\n,"
        spaceSeperator = ""
        repoFilePath = syntax <> fileExt
        withTemplate = contentsWithTemplate template
        contentsWithTemplate :: Maybe String -> String -> String
        contentsWithTemplate (Just template) contents = DT.unpack $ DT.replace "{0}" (toS contents) (toS template)
        contentsWithTemplate Nothing contents = contents

-- | Attempts to pull from the git repository's remote repository.
-- | If the pull fails, the exception is caught and continues to the next step.
runPullGitRemote :: String -> FilePath -> IO ()
runPullGitRemote repoUrl repoPath = do
  Prelude.putStrLn "Attempting to fetch from the remote repository."
  _ <- executeCommand repoPath checkoutMasterCommand
  result <- attempt
  handle result next errorMessage
  where attempt :: IO (Either Prelude.IOError String)
        attempt = try $ executeCommand repoPath pullFromRemoteCommand

        handle :: Either Prelude.IOError String -> IO () -> (Prelude.IOError -> IO ()) -> IO ()
        handle result success err = case (result :: Either Prelude.IOError String) of
                                      Left error -> err error
                                      Right _ -> success
        next :: IO ()
        next = Prelude.putStrLn "Remote repository successfully fetched.\n"

        errorMessage :: Prelude.IOError -> IO ()
        errorMessage err = Prelude.putStrLn $ "Pulling from the remote repository at " <> repoUrl <> " failed with: " <> show err <> ". Proceeding to the next step.\n"

-- | Pushes git commits to the submodule repository's remote.
runPushGitRemote :: FilePath -> IO ()
runPushGitRemote repoPath = do
  Prelude.putStrLn "Updating git remote."
  result <- try $ executeCommand repoPath pushToGitRemoteCommand
  case (result :: Either Prelude.IOError String) of
    Left err -> die $ "Failed to push to remote repository: " <> show err
    Right _ -> Prelude.putStrLn "Successfully updated git remote."

-- | Closes the JSON array and closes the test case file.
runCloseTestCaseFile :: FilePath -> IO ()
runCloseTestCaseFile testCaseFilePath = do
  Prelude.putStrLn $ "Closing " <> testCaseFilePath
  DL.appendFile testCaseFilePath "]\n"

initializeRepoCommand :: String -> String
initializeRepoCommand repoUrl = "rm -rf *; rm -rf .git; git init .; git remote add origin " <> repoUrl <> ";"

addSubmoduleCommand :: String -> FilePath -> String
addSubmoduleCommand repoUrl repoPath = "git submodule add " <> repoUrl <> " " <> " ./" <> repoPath <> ";"

getLastCommitShaCommand :: String
getLastCommitShaCommand = "git log --pretty=format:\"%H\" -n 1;"

gitDiffCommand :: String -> String -> String
gitDiffCommand sha1 sha2 = "git diff " <> sha1 <> ".." <> sha2 <> ";"

checkoutMasterCommand :: String
checkoutMasterCommand = "git checkout master;"

pullFromRemoteCommand :: String
pullFromRemoteCommand = "git pull origin master;"

touchCommand :: FilePath -> String
touchCommand repoFilePath = "touch " <> repoFilePath <> ";"

-- | In order to correctly record syntax examples that include backticks (like JavaScript template strings)
-- | we must first escape them for bash (due to the use of the `echo` system command). Additionally,
-- | we must also escape the escape character `\` in Haskell, hence the double `\\`.
fileWriteCommand :: FilePath -> String -> String
fileWriteCommand repoFilePath contents = "echo \"" <> (escapeBackticks . escapeDoubleQuotes) contents <> "\" > " <> repoFilePath <> ";"
  where
    escapeBackticks = DSUtils.replace "`" "\\`"
    escapeDoubleQuotes = DSUtils.replace "\"" "\\\""

fileAppendCommand :: FilePath -> String -> String
fileAppendCommand repoFilePath contents = "echo \"" <> (escapeBackticks . escapeDoubleQuotes) contents <> "\" >> " <> repoFilePath <> ";"
  where
    escapeBackticks = DSUtils.replace "`" "\\`"
    escapeDoubleQuotes = DSUtils.replace "\"" "\\\""

commitCommand :: String -> String -> String
commitCommand syntax commitMessage = "git add .; git commit -m \"" <> syntax <> ": " <> commitMessage <> "\"" <> ";"

removeCommand :: FilePath -> String
removeCommand repoFilePath = "rm " <> repoFilePath <> ";"

pushToGitRemoteCommand :: String
pushToGitRemoteCommand = "git push origin HEAD;"

mkDirCommand :: FilePath -> String
mkDirCommand repoPath = "mkdir " <> repoPath <> ";"

executeCommand :: FilePath -> String -> IO String
executeCommand repoPath command = readCreateProcess (shell command) { cwd = Just repoPath } ""
