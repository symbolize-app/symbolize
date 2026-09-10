module Dev.Gen.Interpret
  ( interpret,
    Mode (..),
  )
where

import Control.Monad (forM)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (hPut)
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Yaml qualified as Yaml
import Dev.Gen.Command qualified as Command
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.FilePath (FilePath (FilePath))
import Relude.Applicative (pure)
import Relude.Base (Eq, Type, (/=), (==))
import Relude.Bool (Bool (False, True), otherwise, (&&))
import Relude.Container (fromList)
import Relude.File (readFileLBS)
import Relude.Foldable (concat, length, toList)
import Relude.Function (const, ($), (.))
import Relude.Functor (bimap, first, (<$>))
import Relude.Monad (Either (Left, Right), Maybe (Just, Nothing), either, fail, liftIO, (<=<), (>>=))
import Relude.Monoid (Sum, mempty, (<>))
import Relude.Numeric (Integer, (+))
import Relude.Print (putText)
import Relude.String
  ( LByteString,
    String,
    Text,
    decodeUtf8Strict,
    decodeUtf8With,
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
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import System.IO (openTempFile)
import System.Process.Typed qualified as Process
import Toml qualified
import Toml.FromValue qualified as Toml
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
interpret (Exec.Command (Command.ReadJSON Command.YAML filePath)) _ =
  (0,) <$> _loadFromFile filePath _yamlEitherDecode
interpret (Exec.Command (Command.ReadJSON Command.JSON filePath)) _ =
  (0,) <$> _loadFromFile filePath Aeson.eitherDecode
interpret
  (Exec.Command (Command.WriteJSON Command.YAML filePath value))
  mode =
    _dumpToFile
      filePath
      _yamlEitherDecode
      _yamlEncode
      (Aeson.toJSON value)
      mode
      (_formatWithPrettier filePath)
interpret
  (Exec.Command (Command.WriteJSON Command.JSON filePath value))
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
interpret (Exec.Command (Command.ReadTOML filePath)) _ =
  (0,) <$> _loadFromFile filePath _tomlEitherDecode
interpret (Exec.Command Command.ReadVendorTargets) _ =
  (0,) <$> _readVendorTargets

_yamlEitherDecode :: (Aeson.FromJSON a) => LByteString -> Either String a
_yamlEitherDecode = first show . Yaml.decodeEither' . toStrict

_yamlEncode :: (Aeson.ToJSON a) => a -> LByteString
_yamlEncode = toLazy . Yaml.encode

_linesEitherDecode :: LByteString -> Either String (Vector Text)
_linesEitherDecode = bimap show (fromList . lines) . decodeUtf8Strict

_linesEncode :: Vector Text -> LByteString
_linesEncode = encodeUtf8 . unlines . toList

_tomlEitherDecode :: (Toml.FromValue a) => LByteString -> Either String a
_tomlEitherDecode =
  ( \string ->
      case Toml.decode string of
        Toml.Success _warnings value ->
          -- Ignore warnings (includes "unexpected keys")
          Right value
        Toml.Failure errors ->
          Left (show errors)
  )
    <=< ( first show
            . decodeUtf8Strict
        )

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

_readVendorTargets :: (MonadUnliftIO m) => m (Vector (Text, Text))
_readVendorTargets = liftIO $ do
  let vendorRoot = "vendor"
  buckFiles <- findBuckFiles vendorRoot
  targets <- forM buckFiles $ \buckPath -> do
    mContent <- (Just <$> readFileLBS buckPath) `catchAny` const (pure Nothing)
    case mContent >>= (rightToMaybe . decodeUtf8Strict . toStrict) of
      Nothing -> pure []
      Just txt -> do
        let relDir = List.drop (length (vendorRoot :: String) + 1) (FilePath.takeDirectory buckPath)
            relText = Text.replace "\\" "/" (toText relDir)
            names = extractRustLibraryNames txt
        pure [(name, if Text.null relText then ":" <> name else "//" <> relText <> ":" <> name) | name <- names]
  pure (fromList (concat targets))
  where
    rightToMaybe (Right x) = Just x
    rightToMaybe (Left _) = Nothing

    findBuckFiles dir = do
      entries <- Dir.listDirectory dir `catchAny` const (pure [])
      let ignored :: [FilePath.FilePath]
          ignored = [".git", ".tmp", "build", "node_modules", "dist-newstyle", "buck-out"]
      fpaths <- forM entries $ \e -> do
        let full = dir </> e
        isDir <- Dir.doesDirectoryExist full `catchAny` const (pure False)
        if isDir
          then if e `List.elem` ignored then pure [] else findBuckFiles full
          else
            if e == "BUCK" && dir /= "vendor"
              then pure [full]
              else pure []
      pure (concat fpaths)

    extractRustLibraryNames txt =
      go False (lines txt)
      where
        go _ [] = []
        go inLib (l : rest)
          | "rust_library(" `Text.isInfixOf` l = go True rest
          | inLib && "name =" `Text.isInfixOf` l =
              case extractQuoted l of
                Just n -> n : go False rest
                Nothing -> go inLib rest
          | inLib && ")" `Text.isPrefixOf` Text.stripStart l = go False rest
          | otherwise = go inLib rest

        extractQuoted l =
          case Text.splitOn "\"" l of
            (_ : val : _) -> Just val
            _ -> Nothing
