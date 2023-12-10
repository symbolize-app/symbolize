module Dev.Gen.Interpret
  ( interpret,
    Mode (..),
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (hPut)
import Data.Vector (Vector)
import Data.Yaml qualified as Yaml
import Dev.Gen.Command qualified as Command
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.FilePath (FilePath (FilePath))
import Relude.Applicative (pure)
import Relude.Base (Eq, Type, (==))
import Relude.Bool (Bool (True))
import Relude.Container (fromList)
import Relude.File (readFileLBS)
import Relude.Foldable (toList)
import Relude.Function (const, ($), (.))
import Relude.Functor (Bifunctor (bimap), first, (<$>))
import Relude.Monad (Either, Maybe (Just, Nothing), either, fail, liftIO)
import Relude.Monoid (Sum, mempty, (<>))
import Relude.Numeric (Integer)
import Relude.Print (putText)
import Relude.String
  ( LByteString,
    String,
    Text,
    decodeUtf8Strict,
    encodeUtf8,
    fromString,
    lines,
    show,
    toLazy,
    toStrict,
    toString,
    toText,
    unlines,
  )
import System.FilePath qualified as FilePath
import System.IO (openTempFile)
import System.Process.Typed qualified as Process
import UnliftIO.Async (concurrently)
import UnliftIO.Directory (removeFile, renameFile)
import UnliftIO.Exception (bracketOnError, catchAny, tryIO)
import UnliftIO.IO (Handle, hClose)

type Mode :: Type
data Mode where
  Generate :: Mode
  Check :: Mode

interpret :: (MonadUnliftIO m) => Exec.Exec a -> Mode -> m (Sum Integer, a)
interpret (Exec.Pure x) _ =
  pure (mempty, x)
interpret (Exec.Bind x f) mode = do
  (t, x') <- interpret x mode
  (t', y) <- interpret (f x') mode
  pure (t <> t', y)
interpret (Exec.Concurrently x y) mode = do
  ((t, x'), (t', y')) <- concurrently (interpret x mode) (interpret y mode)
  pure (t <> t', (x', y'))
interpret (Exec.Fail s) _ =
  liftIO $ fail s
interpret (Exec.Command (Command.ReadJSON FileFormat.YAML filePath)) _ =
  (0,) <$> _loadFromFile filePath _yamlEitherDecode
interpret (Exec.Command (Command.ReadJSON FileFormat.JSON filePath)) _ =
  (0,) <$> _loadFromFile filePath Aeson.eitherDecode
interpret
  (Exec.Command (Command.WriteJSON FileFormat.YAML filePath value))
  mode =
    _dumpToFile
      filePath
      _yamlEitherDecode
      _yamlEncode
      (Aeson.toJSON value)
      mode
      (_formatWithPrettier filePath)
interpret
  (Exec.Command (Command.WriteJSON FileFormat.JSON filePath value))
  mode =
    _dumpToFile
      filePath
      Aeson.eitherDecode
      Aeson.encode
      (Aeson.toJSON value)
      mode
      (_formatWithPrettier filePath)
interpret (Exec.Command (Command.ReadLines filePath)) _ =
  (0,) <$> _loadFromFile filePath _linesEitherDecode
interpret (Exec.Command (Command.WriteLines filePath value)) mode =
  _dumpToFile
    filePath
    _linesEitherDecode
    _linesEncode
    value
    mode
    (\handle bytes -> liftIO $ hPut handle bytes)

_yamlEitherDecode :: (Aeson.FromJSON a) => LByteString -> Either String a
_yamlEitherDecode = first show . Yaml.decodeEither' . toStrict

_yamlEncode :: (Aeson.ToJSON a) => a -> LByteString
_yamlEncode = toLazy . Yaml.encode

_linesEitherDecode :: LByteString -> Either String (Vector Text)
_linesEitherDecode = bimap show (fromList . lines) . decodeUtf8Strict

_linesEncode :: Vector Text -> LByteString
_linesEncode = encodeUtf8 . unlines . toList

_loadFromFile ::
  (MonadUnliftIO m) =>
  FilePath ->
  (LByteString -> Either String a) ->
  m a
_loadFromFile filePath decode = do
  bytes <- readFileLBS (toString filePath)
  either (liftIO . fail) pure (decode bytes)

_dumpToFile ::
  (Eq a, MonadUnliftIO m) =>
  FilePath ->
  (LByteString -> Either String a) ->
  (a -> LByteString) ->
  a ->
  Mode ->
  (Handle -> LByteString -> m ()) ->
  m (Sum Integer, ())
_dumpToFile filePath decode encode value mode write = do
  oldValue <-
    catchAny
      (Just <$> _loadFromFile filePath decode)
      (const (pure Nothing))
  case (Just value == oldValue, mode) of
    (True, _) -> pure (mempty, ())
    (_, Generate) -> do
      putText ("Writing to " <> toText filePath <> "...\n")
      let bytes = encode value
      _withReplaceFile filePath $ \_filePath handle ->
        write handle bytes
      pure (1, ())
    (_, Check) -> do
      putText ("Skipped writing to " <> toText filePath <> "...\n")
      pure (1, ())

_withReplaceFile ::
  (MonadUnliftIO m) =>
  FilePath ->
  (FilePath -> Handle -> m a) ->
  m a
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

_formatWithPrettier ::
  (MonadUnliftIO m) =>
  FilePath ->
  Handle ->
  LByteString ->
  m ()
_formatWithPrettier filePath handle bytes =
  Process.runProcess_
    . Process.setStdin (Process.byteStringInput bytes)
    . Process.setStdout (Process.useHandleClose handle)
    $ Process.proc "prettier" ["--stdin-filepath", toString filePath]
