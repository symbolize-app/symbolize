module Dev.Gen.Interpret
  ( interpret,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson qualified as Aeson
import Data.Yaml qualified as Yaml
import Dev.Gen.Command qualified as Command
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.FilePath (FilePath (FilePath))
import Relude.Applicative (pure)
import Relude.Base ((==))
import Relude.File (readFileLBS)
import Relude.Function (const, ($), (.))
import Relude.Functor (first, (<$>))
import Relude.Monad (Either, Maybe (Just, Nothing), either, fail, liftIO)
import Relude.Monoid (Sum, mempty, (<>))
import Relude.Numeric (Integer)
import Relude.Print (putText)
import Relude.String (LByteString, String, fromString, show, toLazy, toStrict, toString, toText)
import System.FilePath qualified as FilePath
import System.IO (openTempFile)
import System.Process.Typed qualified as Process
import UnliftIO.Async (concurrently)
import UnliftIO.Directory (removeFile, renameFile)
import UnliftIO.Exception (bracketOnError, catchAny, tryIO)
import UnliftIO.IO (Handle, hClose)

interpret :: (MonadUnliftIO m) => Exec.Exec a -> m (Sum Integer, a)
interpret (Exec.Pure x) =
  pure (mempty, x)
interpret (Exec.Bind x f) = do
  (t, x') <- interpret x
  (t', y) <- interpret (f x')
  pure (t <> t', y)
interpret (Exec.Concurrently x y) = do
  ((t, x'), (t', y')) <- concurrently (interpret x) (interpret y)
  pure (t <> t', (x', y'))
interpret (Exec.Fail s) =
  liftIO $ fail s
interpret (Exec.Command (Command.ReadFile filePath FileFormat.YAML)) =
  (0,) <$> _loadFromFile filePath _yamlEitherDecode
interpret (Exec.Command (Command.ReadFile filePath FileFormat.JSON)) =
  (0,) <$> _loadFromFile filePath Aeson.eitherDecode
interpret (Exec.Command (Command.WriteFile filePath FileFormat.YAML value)) = do
  _dumpToFile filePath _yamlEitherDecode _yamlEncode value
interpret (Exec.Command (Command.WriteFile filePath FileFormat.JSON value)) = do
  _dumpToFile filePath Aeson.eitherDecode Aeson.encode value

_yamlEitherDecode :: (Aeson.FromJSON a) => LByteString -> Either String a
_yamlEitherDecode = first show . Yaml.decodeEither' . toStrict

_yamlEncode :: (Aeson.ToJSON a) => a -> LByteString
_yamlEncode = toLazy . Yaml.encode

_loadFromFile :: (MonadUnliftIO m) => FilePath -> (LByteString -> Either String a) -> m a
_loadFromFile filePath decode = do
  bytes <- readFileLBS (toString filePath)
  either (liftIO . fail) pure (decode bytes)

_dumpToFile ::
  (Aeson.ToJSON a, MonadUnliftIO m) =>
  FilePath ->
  (LByteString -> Either String Aeson.Value) ->
  (Aeson.Value -> LByteString) ->
  a ->
  m (Sum Integer, ())
_dumpToFile filePath decode encode value = do
  -- TODO Read and compare `Value` to skip writing
  -- let value' = Aeson.toJSON value
  let value' = Aeson.toJSON value :: Aeson.Value
  oldValue <-
    catchAny
      (Just <$> _loadFromFile filePath decode)
      (const (pure Nothing))
  if Just value' == oldValue
    then pure (mempty, ())
    else do
      putText ("Writing to " <> toText filePath <> "...\n")
      let bytes = encode value'
      _withReplaceFile filePath $ \_filePath handle ->
        Process.runProcess_
          . Process.setStdin (Process.byteStringInput bytes)
          . Process.setStdout (Process.useHandleClose handle)
          $ Process.proc "prettier" ["--stdin-filepath", toString filePath]
      pure (1, ())

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
