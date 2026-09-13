module Dev.Gen.Vendor.Check
  ( checkDupLedger,
    checkGleamDependencies,
    checkNodeJson,
    checkVendorHygiene,
    runVendorCheck,
    splitDirName,
  )
where

import Control.Monad (filterM, forM, mapM)
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Data.Aeson qualified as Aeson
import Data.Char (isDigit, isHexDigit)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Relude.Applicative (pure, (<*>))
import Relude.Base (Char, Eq, Ord, Show, (/=), (<), (==), (>), (>=))
import Relude.Bool (Bool (False, True), not, otherwise, (&&), (||))
import Relude.Container (fromList)
import Relude.File (readFileLBS)
import Relude.Foldable (any, concat, concatMap, elem, for_, length, notElem, null, toList)
import Relude.Function (const, flip, ($), (.))
import Relude.Functor (fmap, (<$>))
import Relude.Monad (Either (Left, Right), Maybe (Just, Nothing), either, fromLeft, fromMaybe, fromRight, maybe, (>>=))
import Relude.Monoid ((<>))
import Relude.Numeric (Int, Integer, (+), (-))
import Relude.Print (putTextLn)
import Relude.String (String, Text, decodeUtf8Strict, show, toStrict, toString, toText)
import System.Directory qualified as Dir
import System.FilePath (FilePath, splitDirectories, takeFileName, (</>))
import Toml qualified
import Toml.FromValue qualified as Toml
import UnliftIO.Exception (catchAny)

-- | Parses "<name>-<version>" where version starts with a digit following a hyphen.
splitDirName :: Text -> Maybe (Text, Text)
splitDirName txt =
  let str = toString txt
   in case findDashDigit str 0 of
        Just idx ->
          let name = toText (List.take idx str)
              ver = toText (List.drop (idx + 1) str)
           in Just (name, ver)
        Nothing -> Nothing
  where
    findDashDigit (c1 : c2 : cs) i
      | c1 == '-' && isDigit c2 && i > 0 = Just i
      | otherwise = findDashDigit (c2 : cs) (i + 1)
    findDashDigit _ _ = Nothing

data DuplicateEntry = DuplicateEntry
  { name :: Text,
    snapshots :: [Text],
    consumers :: [Text],
    justification :: Text
  }
  deriving stock (Show, Eq)

instance Toml.FromValue DuplicateEntry where
  fromValue =
    Toml.parseTableFromValue $
      DuplicateEntry
        <$> Toml.reqKey "name"
        <*> Toml.reqKey "snapshots"
        <*> Toml.reqKey "consumers"
        <*> Toml.reqKey "justification"

data DupLedger = DupLedger
  { version :: Integer,
    duplicate :: Maybe [DuplicateEntry]
  }
  deriving stock (Show, Eq)

instance Toml.FromValue DupLedger where
  fromValue =
    Toml.parseTableFromValue $
      DupLedger
        <$> Toml.reqKey "version"
        <*> Toml.optKey "duplicate"

data VendorMeta = VendorMeta
  { url :: Text,
    gitRef :: Text
  }
  deriving stock (Show, Eq)

instance Toml.FromValue VendorMeta where
  fromValue =
    Toml.parseTableFromValue $
      VendorMeta
        <$> Toml.reqKey "url"
        <*> Toml.reqKey "ref"

checkDupLedger :: (MonadUnliftIO m) => FilePath -> m (Either [Text] Int)
checkDupLedger repoRoot = do
  let vendorDir = repoRoot </> "vendor"
      dupPath = vendorDir </> "dup.toml"
  dupExists <- liftIO $ Dir.doesFileExist dupPath
  if not dupExists
    then pure $ Left ["vendor/dup.toml does not exist"]
    else do
      mDupContent <- liftIO $ (Just <$> readFileLBS dupPath) `catchAny` const (pure Nothing)
      case mDupContent >>= (rightToMaybe . decodeUtf8Strict . toStrict) of
        Nothing -> pure $ Left ["Failed to read vendor/dup.toml"]
        Just dupText ->
          case Toml.decode dupText :: Toml.Result String DupLedger of
            Toml.Failure errs -> pure $ Left ["vendor/dup.toml parsing failed: " <> (show errs :: Text)]
            Toml.Success _ ledger ->
              if ledger.version /= 1
                then pure $ Left ["vendor/dup.toml must declare version = 1"]
                else validateSnapshots vendorDir ledger
  where
    validateSnapshots vendorDir ledger = do
      allEntries <- liftIO $ Dir.listDirectory vendorDir
      pkgDirs <- flip filterM allEntries $ \e -> do
        let full = vendorDir </> e
        isDir <- liftIO $ Dir.doesDirectoryExist full
        pure (isDir && not ("." `List.isPrefixOf` e))

      varResults <- forM pkgDirs (checkSnapshotDir vendorDir)
      let (dirErrors, snapshotPairs) = partitionEithers varResults
      if not (null dirErrors)
        then pure $ Left dirErrors
        else do
          let snapshotGroups =
                List.foldl'
                  (\acc (pkg, dirName) -> Map.insertWith (<>) pkg [dirName] acc)
                  Map.empty
                  snapshotPairs
              actualDuplicates =
                Map.filter (\dirs -> length dirs > 1) (Map.map List.sort snapshotGroups)
              declared = fromMaybe [] ledger.duplicate
          case checkDeclaredDuplicates declared actualDuplicates of
            Left errs -> pure $ Left errs
            Right () -> pure $ Right (length pkgDirs)

    checkSnapshotDir vendorDir dirName = do
      let fullDir = vendorDir </> dirName
          metaPath = fullDir </> ".vendor.toml"
          nameText = toText dirName
      case splitDirName nameText of
        Nothing -> pure $ Left ("Directory " <> nameText <> " does not match '<name>-<version>' format")
        Just (pkgName, _ver) -> do
          metaExists <- liftIO $ Dir.doesFileExist metaPath
          if not metaExists
            then pure $ Left (toText metaPath <> " is missing")
            else do
              mContent <- liftIO $ (Just <$> readFileLBS metaPath) `catchAny` const (pure Nothing)
              case mContent >>= (rightToMaybe . decodeUtf8Strict . toStrict) of
                Nothing -> pure $ Left ("Failed to read " <> toText metaPath)
                Just metaText ->
                  case Toml.decode metaText :: Toml.Result String VendorMeta of
                    Toml.Failure errs -> pure $ Left (toText metaPath <> " invalid: " <> (show errs :: Text))
                    Toml.Success _ meta ->
                      if Text.null meta.url
                        then pure $ Left (toText metaPath <> " must contain a non-empty url")
                        else
                          if Text.length meta.gitRef /= 40 || not (Text.all isHexDigit meta.gitRef)
                            then pure $ Left (toText metaPath <> " must contain a full 40-character Git ref")
                            else pure $ Right (pkgName, nameText)

    checkDeclaredDuplicates declared actualDuplicates =
      let declaredNames = [e.name | e <- declared]
          dupNames = declaredNames List.\\ List.nub declaredNames
       in if not (null dupNames)
            then Left ["Duplicate entry declared more than once in vendor/dup.toml: " <> Text.intercalate ", " dupNames]
            else
              let declaredErrors = [err | e <- declared, Just err <- [checkEntry e actualDuplicates]]
                  missingActual = Map.keys actualDuplicates List.\\ declaredNames
               in if not (null declaredErrors)
                    then Left declaredErrors
                    else
                      if not (null missingActual)
                        then Left ["vendor/dup.toml is missing entries for duplicated snapshots: " <> Text.intercalate ", " missingActual]
                        else Right ()

    checkEntry entry actualDuplicates =
      case Map.lookup entry.name actualDuplicates of
        Nothing -> Just ("invalid duplicate entry '" <> entry.name <> "': does not correspond to any duplicated snapshot directory in vendor/")
        Just actualDirs ->
          let listed = entry.snapshots
           in if length listed < 2 || length (List.nub listed) /= length listed
                then Just ("invalid duplicate entry '" <> entry.name <> "': snapshots must be at least two unique directories")
                else
                  if List.sort listed /= actualDirs
                    then Just ("invalid duplicate entry '" <> entry.name <> "': snapshots list does not match actual retained snapshots")
                    else
                      if null entry.consumers || any Text.null (fmap Text.strip entry.consumers)
                        then Just ("invalid duplicate entry '" <> entry.name <> "': consumers must be non-empty")
                        else
                          if Text.length (Text.strip entry.justification) < 20
                            then Just ("invalid duplicate entry '" <> entry.name <> "': justification is missing or too brief")
                            else
                              if "minor vendored source changes needed" `Text.isInfixOf` Text.toLower entry.justification
                                then Just ("invalid duplicate entry '" <> entry.name <> "': uses a placeholder justification")
                                else Nothing

data NodeJson = NodeJson
  { version :: Maybe Integer,
    imports :: Map Text Text
  }
  deriving stock (Show, Eq)

instance Aeson.FromJSON NodeJson where
  parseJSON = Aeson.withObject "NodeJson" $ \obj ->
    NodeJson
      <$> obj
      Aeson..:? "version"
      <*> obj Aeson..: "imports"

checkNodeJson :: (MonadUnliftIO m) => FilePath -> m (Either [Text] Int)
checkNodeJson repoRoot = do
  let nodeJsonPath = repoRoot </> "vendor" </> "node.json"
  exists <- liftIO $ Dir.doesFileExist nodeJsonPath
  if not exists
    then pure $ Left ["vendor/node.json must exist"]
    else do
      mContent <- liftIO $ (Just <$> readFileLBS nodeJsonPath) `catchAny` const (pure Nothing)
      case (mContent >>= Aeson.decode) :: Maybe NodeJson of
        Nothing -> pure $ Left ["Failed to parse vendor/node.json"]
        Just nodeJson ->
          if Map.null nodeJson.imports
            then pure $ Left ["vendor/node.json must contain a non-empty 'imports' object"]
            else do
              missingFiles <- flip filterM (Map.toList nodeJson.imports) $ \(_spec, pathText) -> do
                let full = repoRoot </> toString pathText
                fExists <- liftIO $ Dir.doesFileExist full
                dExists <- liftIO $ Dir.doesDirectoryExist full
                pure (not fExists && not dExists)
              if not (null missingFiles)
                then
                  pure $
                    Left
                      [ "vendor/node.json mapping '" <> spec <> "' -> '" <> p <> "' does not exist on disk"
                        | (spec, p) <- missingFiles
                      ]
                else pure $ Right (Map.size nodeJson.imports)

data GleamConfig = GleamConfig
  { dependencies :: Maybe (Map Text Toml.Value),
    devDependencies :: Maybe (Map Text Toml.Value)
  }

instance Toml.FromValue GleamConfig where
  fromValue =
    Toml.parseTableFromValue $
      GleamConfig
        <$> Toml.optKey "dependencies"
        <*> Toml.optKey "dev-dependencies"

data GleamManifestPackage = GleamManifestPackage
  { name :: Maybe Text,
    source :: Maybe Text
  }

instance Toml.FromValue GleamManifestPackage where
  fromValue =
    Toml.parseTableFromValue $
      GleamManifestPackage
        <$> Toml.optKey "name"
        <*> Toml.optKey "source"

newtype GleamLockedManifest = GleamLockedManifest
  { packages :: Maybe [GleamManifestPackage]
  }

instance Toml.FromValue GleamLockedManifest where
  fromValue =
    Toml.parseTableFromValue $
      GleamLockedManifest
        <$> Toml.optKey "packages"

checkGleamDependencies :: (MonadUnliftIO m) => FilePath -> m (Either [Text] Int)
checkGleamDependencies repoRoot = do
  gleamFiles <- liftIO $ findFilesRecursive repoRoot "gleam.toml"
  manifestFiles <- liftIO $ findFilesRecursive repoRoot "manifest.toml"
  gleamErrors <- concat <$> mapM checkGleamToml gleamFiles
  manifestErrors <- concat <$> mapM checkManifestToml manifestFiles
  let allErrors = gleamErrors <> manifestErrors
  if not (null allErrors)
    then pure $ Left allErrors
    else pure $ Right (length gleamFiles)
  where
    findFilesRecursive dir targetName = do
      entries <- Dir.listDirectory dir `catchAny` const (pure [])
      let ignored :: [FilePath]
          ignored = [".git", ".tmp", "build", "node_modules", "dist-newstyle", "buck-out"]
      validEntries <- flip filterM entries $ \e -> do
        let full = dir </> e
        isDir <- Dir.doesDirectoryExist full `catchAny` const (pure False)
        pure (isDir && e `notElem` ignored)
      let matched = [dir </> targetName | targetName `elem` entries]
      nested <- concat <$> mapM (\d -> findFilesRecursive (dir </> d) targetName) validEntries
      pure (matched <> nested)

    checkGleamToml path = do
      mContent <- liftIO $ (Just <$> readFileLBS path) `catchAny` const (pure Nothing)
      pure $ case mContent >>= (rightToMaybe . decodeUtf8Strict . toStrict) of
        Nothing -> ["Failed to read " <> toText path]
        Just txt ->
          case Toml.decode txt :: Toml.Result String GleamConfig of
            Toml.Failure _ -> ["Failed to parse TOML in " <> toText path]
            Toml.Success _ config ->
              validateSection "dependencies" (fromMaybe Map.empty config.dependencies)
                <> validateSection "dev-dependencies" (fromMaybe Map.empty config.devDependencies)

    validateSection sec table =
      concatMap (validateDep sec) (Map.toList table)

    validateDep sec (k, val) =
      case val of
        Toml.String _ -> [sec <> "." <> k <> " uses a registry version constraint"]
        Toml.Table t ->
          case Map.lookup "path" t of
            Just (Toml.String p) | not (null p) -> []
            _ -> [sec <> "." <> k <> " must use a local path dependency"]
        _ -> [sec <> "." <> k <> " must use a local path dependency"]

    checkManifestToml path = do
      mContent <- liftIO $ (Just <$> readFileLBS path) `catchAny` const (pure Nothing)
      pure $ case mContent >>= (rightToMaybe . decodeUtf8Strict . toStrict) of
        Nothing -> ["Failed to read " <> toText path]
        Just txt ->
          case Toml.decode txt :: Toml.Result String GleamLockedManifest of
            Toml.Failure _ -> ["Failed to parse TOML in " <> toText path]
            Toml.Success _ manifest ->
              concatMap validatePkg (fromMaybe [] manifest.packages)

    validatePkg pkg =
      case pkg.source of
        Just "local" -> []
        Just src ->
          let pkgName = fromMaybe "<unnamed>" pkg.name
           in [pkgName <> " has non-local source " <> src]
        Nothing -> ["package has missing source in manifest"]

checkVendorHygiene :: (MonadUnliftIO m) => FilePath -> m (Either [Text] ())
checkVendorHygiene repoRoot = do
  let vendorDir = repoRoot </> "vendor"
  badFiles <- liftIO $ findForbiddenFiles vendorDir
  specificErrors <- liftIO checkSpecificFiles
  puppeteerRuntimeExists <- liftIO $ Dir.doesDirectoryExist (vendorDir </> "puppeteer-25.9.0" </> "runtime")
  let puppeteerErrors =
        ["Found forbidden runtime bundle directory in vendor/puppeteer-25.9.0/runtime" | puppeteerRuntimeExists]
      allErrors = badFiles <> specificErrors <> puppeteerErrors
  if not (null allErrors)
    then pure $ Left allErrors
    else pure $ Right ()
  where
    forbiddenNames :: [FilePath]
    forbiddenNames = ["AGENTS.md", "CLAUDE.md", "CODEX.md", "SKILL.md", ".agents", ".claude", ".gitattributes", "symbolize.mjs"]

    forbiddenDirs :: [FilePath]
    forbiddenDirs = [".git", "node_modules"]

    findForbiddenFiles dir = do
      entries <- Dir.listDirectory dir `catchAny` const (pure [])
      let dirBad = [toText (dir </> e) <> " is forbidden in vendor" | e <- entries, e `elem` forbiddenNames]
      subdirs <- flip filterM entries $ \e -> do
        let full = dir </> e
        Dir.doesDirectoryExist full `catchAny` const (pure False)
      let forbiddenSubdirs = [toText (dir </> e) <> " directory is forbidden in vendor" | e <- subdirs, e `elem` forbiddenDirs]
          validSubdirs = [e | e <- subdirs, e `notElem` forbiddenDirs]
      nested <- concat <$> mapM (\sd -> findForbiddenFiles (dir </> sd)) validSubdirs
      pure (dirBad <> forbiddenSubdirs <> nested)

    checkSpecificFiles = do
      let required =
            [ repoRoot </> "vendor" </> "gleam_stdlib-1.0.5" </> "gleam.toml",
              repoRoot </> "vendor" </> "gleam_stdlib-1.0.5" </> "src" </> "gleam" </> "io.gleam",
              repoRoot </> "vendor" </> "gleam_stdlib-1.0.5" </> "src" </> "gleam_stdlib.mjs",
              repoRoot </> "vendor" </> "sqlite3-3.46.0" </> "Makefile.linux-gcc",
              repoRoot </> "vendor" </> "sqlite3-3.46.0" </> "main.mk",
              repoRoot </> "vendor" </> "sqlite3-3.46.0" </> "src" </> "sqlite.h.in"
            ]
      missing <- flip filterM required $ \p -> do
        exists <- Dir.doesFileExist p `catchAny` const (pure False)
        pure (not exists)
      pure [toText p <> " is missing" | p <- missing]

runVendorCheck :: (MonadUnliftIO m) => FilePath -> m Bool
runVendorCheck repoRoot = do
  putTextLn "Running vendor closure and integrity checks..."
  dupRes <- checkDupLedger repoRoot
  nodeRes <- checkNodeJson repoRoot
  gleamRes <- checkGleamDependencies repoRoot
  hygieneRes <- checkVendorHygiene repoRoot
  let allErrors =
        fromLeft [] dupRes
          <> fromLeft [] nodeRes
          <> fromLeft [] gleamRes
          <> fromLeft [] hygieneRes
  if not (null allErrors)
    then do
      putTextLn "❌ Vendor checks failed:\n"
      for_ allErrors $ \err ->
        putTextLn $ "  - " <> err
      pure False
    else do
      let count = fromRight 0 dupRes
      putTextLn $ "✅ Vendor closure verified: " <> show count <> " snapshots, dup.toml, node.json, Gleam paths valid."
      pure True

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Right b) = Just b
rightToMaybe (Left _) = Nothing

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = List.foldr (either left right) ([], [])
  where
    left a (ls, rs) = (a : ls, rs)
    right b (ls, rs) = (ls, b : rs)
