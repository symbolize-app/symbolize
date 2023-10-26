module Dev.Gen.Interpret
  ( interpret,
  )
where

import Data.Aeson qualified as Aeson
import Data.Yaml qualified as Yaml
import Dev.Gen.Command qualified as Command
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.FilePath (FilePath (FilePath))
import Relude.Applicative (pure)
import Relude.File (readFileLBS, writeFileBS)
import Relude.Function (($), (.))
import Relude.Functor (first)
import Relude.Monad (Either, either, fail, liftIO, (>>=))
import Relude.Monoid ((<>))
import Relude.Print (putText)
import Relude.String (LByteString, String, fromString, show, toStrict, toString, toText)
import System.FilePath qualified as FilePath
import System.IO (openTempFile)
import System.Process.Typed qualified as Process
import UnliftIO (Handle, MonadUnliftIO, bracketOnError, hClose)
import UnliftIO.Async (concurrently)
import UnliftIO.Directory (removeFile, renameFile)
import UnliftIO.Exception (tryIO)

interpret :: (MonadUnliftIO m) => Exec.Exec a -> m a
interpret (Exec.Pure x) =
  pure x
interpret (Exec.Bind x f) =
  interpret x >>= interpret . f
interpret (Exec.Concurrently x y) =
  concurrently (interpret x) (interpret y)
interpret (Exec.Fail s) =
  liftIO $ fail s
interpret (Exec.Command (Command.ReadFile filePath FileFormat.YAML)) = do
  _loadFromFile filePath (first show . Yaml.decodeEither' . toStrict)
interpret (Exec.Command (Command.ReadFile filePath FileFormat.JSON)) =
  _loadFromFile filePath Aeson.eitherDecode
interpret (Exec.Command (Command.WriteFile filePath FileFormat.YAML value)) = do
  writeFileBS (toString filePath) $ Yaml.encode value
interpret (Exec.Command (Command.WriteFile filePath FileFormat.JSON value)) = do
  _dumpToFile filePath Aeson.encode value

_loadFromFile :: (MonadUnliftIO m) => FilePath -> (LByteString -> Either String a) -> m a
_loadFromFile filePath load = do
  bytes <- readFileLBS (toString filePath)
  either (liftIO . fail) pure (load bytes)

_dumpToFile :: (MonadUnliftIO m) => FilePath -> (a -> LByteString) -> a -> m ()
_dumpToFile filePath dump value = do
  -- TODO Read and compare `Value` to skip writing
  putText ("Writing to " <> toText filePath <> "...\n")
  let bytes = dump value
  _withReplaceFile filePath $ \_filePath handle -> do
    Process.runProcess_
      . Process.setStdin (Process.byteStringInput bytes)
      . Process.setStdout (Process.useHandleClose handle)
      $ Process.proc "prettier" ["--stdin-filepath", toString filePath]

_withReplaceFile :: (MonadUnliftIO m) => FilePath -> (FilePath -> Handle -> m a) -> m a
_withReplaceFile filePath action =
  let filePath' = toString filePath
      (tempDir, tempTemplate) = FilePath.splitFileName filePath'
   in bracketOnError
        (liftIO (openTempFile tempDir tempTemplate))
        ( \(tempName, handle) -> liftIO $ do
            hClose handle
            tryIO (removeFile tempName)
        )
        ( \(tempName, handle) -> do
            result <- action (FilePath (fromString tempName)) handle
            hClose handle
            renameFile tempName filePath'
            pure result
        )
