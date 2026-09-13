module Dev.Gen.Vendor.License
  ( Spdx (..),
    allowedLicenses,
    allowedWithExceptions,
    detectPackageLicense,
    evalSpdx,
    parseSpdx,
    runLicenseCheck,
  )
where

import Control.Monad (filterM)
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isSpace)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Relude.Applicative (pure, (<*>))
import Relude.Base (Char, Eq, Ord, Show, (/=), (<), (==))
import Relude.Bool (Bool (False, True), not, otherwise, (&&), (||))
import Relude.Container (fromList)
import Relude.File (readFileLBS)
import Relude.Foldable (for_, length, null, toList)
import Relude.Function (const, flip, ($), (.))
import Relude.Functor (fmap, (<$>))
import Relude.Monad (Either (Left, Right), Maybe (Just, Nothing), either, maybe, (>>=))
import Relude.Monoid ((<>))
import Relude.Print (putTextLn)
import Relude.String (String, Text, decodeUtf8Strict, lines, show, toStrict, toString, toText)
import System.Directory qualified as Dir
import System.FilePath (FilePath, takeFileName, (</>))
import Toml qualified
import Toml.FromValue qualified as Toml
import UnliftIO.Exception (catchAny)

data Spdx
  = Lic Text
  | With Text Text
  | And Spdx Spdx
  | Or Spdx Spdx
  deriving stock (Show, Eq)

allowedLicenses :: Set Text
allowedLicenses =
  Set.fromList
    [ "MIT",
      "Apache-2.0",
      "BSD-3-Clause",
      "BSD-2-Clause",
      "0BSD",
      "ISC",
      "Unicode-DFS-2016",
      "Unlicense",
      "blessing",
      "LicenseRef-ring",
      "BSL-1.0"
    ]

allowedWithExceptions :: Set (Text, Text)
allowedWithExceptions =
  Set.fromList
    [ ("Apache-2.0", "LLVM-exception")
    ]

packageOverrides :: Map Text (Text, Text)
packageOverrides =
  Map.fromList
    [ ("sqlite3-3.46.0", ("blessing", "SQLite3 public domain blessing")),
      ("ring-0.17.7", ("LicenseRef-ring", "Ring custom OpenSSL/ISC hybrid license"))
    ]

data Token
  = OpenParen
  | CloseParen
  | TokAnd
  | TokOr
  | TokWith
  | TokIdent Text
  deriving stock (Show, Eq)

tokenizeSpdx :: Text -> [Token]
tokenizeSpdx raw =
  let normalized = Text.replace "/" " OR " raw
   in go (toString normalized)
  where
    go [] = []
    go ('(' : cs) = OpenParen : go cs
    go (')' : cs) = CloseParen : go cs
    go (c : cs)
      | isSpace c = go cs
      | otherwise =
          let (word, rest) = List.span (\x -> not (isSpace x) && x /= '(' && x /= ')') (c : cs)
              tok = case word of
                "AND" -> TokAnd
                "OR" -> TokOr
                "WITH" -> TokWith
                w -> TokIdent (toText w)
           in tok : go rest

parseSpdx :: Text -> Maybe Spdx
parseSpdx text = do
  let tokens = tokenizeSpdx text
  (expr, rest) <- parseOr tokens
  case rest of
    [] -> Just expr
    _ -> Nothing

parseOr :: [Token] -> Maybe (Spdx, [Token])
parseOr toks = do
  (left, rest) <- parseAnd toks
  case rest of
    TokOr : rest' -> do
      (right, rest'') <- parseOr rest'
      pure (Or left right, rest'')
    _ -> pure (left, rest)

parseAnd :: [Token] -> Maybe (Spdx, [Token])
parseAnd toks = do
  (left, rest) <- parseTerm toks
  case rest of
    TokAnd : rest' -> do
      (right, rest'') <- parseAnd rest'
      pure (And left right, rest'')
    _ -> pure (left, rest)

parseTerm :: [Token] -> Maybe (Spdx, [Token])
parseTerm (OpenParen : rest) = do
  (inner, rest') <- parseOr rest
  case rest' of
    CloseParen : rest'' -> pure (inner, rest'')
    _ -> Nothing
parseTerm (TokIdent lic : TokWith : TokIdent exc : rest) =
  pure (With lic exc, rest)
parseTerm (TokIdent lic : rest) =
  pure (Lic lic, rest)
parseTerm _ = Nothing

evalSpdx :: Spdx -> Bool
evalSpdx (Lic lic) = Set.member lic allowedLicenses
evalSpdx (With lic exc) =
  Set.member (lic, exc) allowedWithExceptions || Set.member lic allowedLicenses
evalSpdx (And a b) = evalSpdx a && evalSpdx b
evalSpdx (Or a b) = evalSpdx a || evalSpdx b

detectLicenseFromText :: Text -> Maybe Text
detectLicenseFromText content
  | "MIT License" `Text.isInfixOf` content || "Permission is hereby granted, free of charge" `Text.isInfixOf` content =
      Just "MIT"
  | "Apache License" `Text.isInfixOf` content || "http://www.apache.org/licenses/LICENSE-2.0" `Text.isInfixOf` content =
      Just "Apache-2.0"
  | "ISC License" `Text.isInfixOf` content || "Permission to use, copy, modify, and/or distribute this software" `Text.isInfixOf` content =
      Just "ISC"
  | "Redistribution and use in source and binary forms" `Text.isInfixOf` content =
      if "Neither the name of" `Text.isInfixOf` content
        then Just "BSD-3-Clause"
        else Just "BSD-2-Clause"
  | "This is free and unencumbered software released into the public domain" `Text.isInfixOf` content =
      Just "Unlicense"
  | "Unicode, Inc. License Agreement" `Text.isInfixOf` content || "Unicode-DFS-2016" `Text.isInfixOf` content =
      Just "Unicode-DFS-2016"
  | otherwise = Nothing

detectPackageLicense :: (MonadUnliftIO m) => FilePath -> m (Maybe (Text, Text))
detectPackageLicense pkgPath = do
  let pkgDirName = toText (takeFileName pkgPath)
  case Map.lookup pkgDirName packageOverrides of
    Just (lic, reason) -> pure $ Just (lic, "override (" <> reason <> ")")
    Nothing -> do
      -- 1. Cargo.toml
      cargoRes <- detectFromCargo pkgPath
      case cargoRes of
        Just res -> pure (Just res)
        Nothing -> do
          -- 2. package.json
          pkgJsonRes <- detectFromPackageJson pkgPath
          case pkgJsonRes of
            Just res -> pure (Just res)
            Nothing -> do
              -- 3. gleam.toml
              gleamRes <- detectFromGleam pkgPath
              case gleamRes of
                Just res -> pure (Just res)
                Nothing -> do
                  -- 4. License files
                  licFileRes <- detectFromLicenseFiles pkgPath
                  case licFileRes of
                    Just res -> pure (Just res)
                    Nothing ->
                      if "sqlite3" `Text.isInfixOf` pkgDirName
                        then pure $ Just ("blessing", "SQLite3 blessing dedication")
                        else pure Nothing

newtype CargoPackage = CargoPackage
  { license :: Maybe Text
  }

instance Toml.FromValue CargoPackage where
  fromValue = Toml.parseTableFromValue (CargoPackage <$> Toml.optKey "license")

newtype CargoWorkspace = CargoWorkspace
  { package :: Maybe CargoPackage
  }

instance Toml.FromValue CargoWorkspace where
  fromValue = Toml.parseTableFromValue (CargoWorkspace <$> Toml.optKey "package")

data CargoManifest = CargoManifest
  { package :: Maybe CargoPackage,
    workspace :: Maybe CargoWorkspace
  }

instance Toml.FromValue CargoManifest where
  fromValue =
    Toml.parseTableFromValue
      ( CargoManifest
          <$> Toml.optKey "package"
          <*> Toml.optKey "workspace"
      )

detectFromCargo :: (MonadUnliftIO m) => FilePath -> m (Maybe (Text, Text))
detectFromCargo dir = do
  let cargoPath = dir </> "Cargo.toml"
  exists <- liftIO $ Dir.doesFileExist cargoPath
  if exists
    then do
      mLic <- readCargoLicense cargoPath
      case mLic of
        Just lic -> pure $ Just (lic, "Cargo.toml [package.license]")
        Nothing -> checkSubcrates dir
    else checkSubcrates dir
  where
    readCargoLicense path = liftIO $ do
      mContent <- (Just <$> readFileLBS path) `catchAny` const (pure Nothing)
      pure $ do
        bs <- mContent
        txt <- rightToMaybe (decodeUtf8Strict (toStrict bs))
        case Toml.decode txt of
          Toml.Success _ (manifest :: CargoManifest) -> extractCargoLic manifest
          Toml.Failure _ -> Nothing

    extractCargoLic (CargoManifest mPkg mWs) =
      case mPkg >>= (.license) of
        Just lic -> Just lic
        Nothing -> mWs >>= (.package) >>= (.license)

    checkSubcrates root = do
      subdirs <- listSubdirs root
      findFirstSubcrate subdirs

    findFirstSubcrate [] = pure Nothing
    findFirstSubcrate (sd : sds) = do
      let subCargo = sd </> "Cargo.toml"
      exists <- liftIO $ Dir.doesFileExist subCargo
      if exists
        then do
          mLic <- readCargoLicense subCargo
          case mLic of
            Just lic -> do
              rel <- liftIO $ Dir.makeRelativeToCurrentDirectory sd
              pure $ Just (lic, "Subcrate Cargo.toml in " <> toText rel)
            Nothing -> findFirstSubcrate sds
        else do
          nested <- listSubdirs sd
          mNested <- findFirstSubcrate nested
          case mNested of
            Just res -> pure (Just res)
            Nothing -> findFirstSubcrate sds

detectFromPackageJson :: (MonadUnliftIO m) => FilePath -> m (Maybe (Text, Text))
detectFromPackageJson dir = do
  let pkgJsonPath = dir </> "package.json"
  exists <- liftIO $ Dir.doesFileExist pkgJsonPath
  if exists
    then do
      mLic <- readPackageJsonLicense pkgJsonPath
      case mLic of
        Just lic -> pure $ Just (lic, "package.json license")
        Nothing -> checkSubpackages dir
    else checkSubpackages dir
  where
    readPackageJsonLicense path = liftIO $ do
      mContent <- (Just <$> readFileLBS path) `catchAny` const (pure Nothing)
      pure $ do
        bs <- mContent
        val <- Aeson.decode bs
        extractPackageLic val

    extractPackageLic (Aeson.Object obj) =
      case KeyMap.lookup "license" obj of
        Just (Aeson.String s) -> Just s
        Just (Aeson.Object licObj) ->
          case KeyMap.lookup "type" licObj of
            Just (Aeson.String t) -> Just t
            _ -> Nothing
        _ -> Nothing
    extractPackageLic _ = Nothing

    checkSubpackages root = do
      subdirs <- listSubdirs root
      findFirstSubpkg subdirs

    findFirstSubpkg [] = pure Nothing
    findFirstSubpkg (sd : sds) = do
      let subJson = sd </> "package.json"
      exists <- liftIO $ Dir.doesFileExist subJson
      if exists
        then do
          mLic <- readPackageJsonLicense subJson
          case mLic of
            Just lic -> do
              rel <- liftIO $ Dir.makeRelativeToCurrentDirectory sd
              pure $ Just (lic, "Subpackage package.json in " <> toText rel)
            Nothing -> findFirstSubpkg sds
        else do
          nested <- listSubdirs sd
          mNested <- findFirstSubpkg nested
          case mNested of
            Just res -> pure (Just res)
            Nothing -> findFirstSubpkg sds

newtype GleamManifest = GleamManifest
  { licences :: Maybe [Text]
  }

instance Toml.FromValue GleamManifest where
  fromValue = Toml.parseTableFromValue (GleamManifest <$> Toml.optKey "licences")

detectFromGleam :: (MonadUnliftIO m) => FilePath -> m (Maybe (Text, Text))
detectFromGleam dir = do
  let gleamPath = dir </> "gleam.toml"
  exists <- liftIO $ Dir.doesFileExist gleamPath
  if exists
    then liftIO $ do
      mContent <- (Just <$> readFileLBS gleamPath) `catchAny` const (pure Nothing)
      pure $ do
        bs <- mContent
        txt <- rightToMaybe (decodeUtf8Strict (toStrict bs))
        case Toml.decode txt of
          Toml.Success _ (manifest :: GleamManifest) ->
            case manifest.licences of
              Just lics | not (null lics) -> Just (Text.intercalate " OR " lics, "gleam.toml licences")
              _ -> Nothing
          Toml.Failure _ -> Nothing
    else pure Nothing

detectFromLicenseFiles :: (MonadUnliftIO m) => FilePath -> m (Maybe (Text, Text))
detectFromLicenseFiles dir = liftIO $ do
  entries <- Dir.listDirectory dir `catchAny` const (pure [])
  let licFiles = List.sort (List.filter isLicenseFile entries)
  checkFiles licFiles
  where
    isLicenseFile f =
      let upper = Text.toUpper (toText f)
       in Text.isPrefixOf "LICENSE" upper || Text.isPrefixOf "COPYING" upper

    checkFiles [] = pure Nothing
    checkFiles (f : fs) = do
      mContent <- (Just <$> readFileLBS (dir </> f)) `catchAny` const (pure Nothing)
      case mContent >>= (rightToMaybe . decodeUtf8Strict . toStrict) of
        Just content ->
          case detectLicenseFromText content of
            Just lic -> pure $ Just (lic, "License file " <> toText f)
            Nothing -> checkFiles fs
        Nothing -> checkFiles fs

listSubdirs :: (MonadUnliftIO m) => FilePath -> m [FilePath]
listSubdirs dir = liftIO $ do
  entries <- Dir.listDirectory dir `catchAny` const (pure [])
  subdirs <- flip filterM entries $ \e -> do
    let full = dir </> e
    Dir.doesDirectoryExist full `catchAny` const (pure False)
  pure $ List.sort [dir </> e | e <- subdirs, not ("." `List.isPrefixOf` e)]

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Right b) = Just b
rightToMaybe (Left _) = Nothing

runLicenseCheck :: (MonadUnliftIO m) => FilePath -> m Bool
runLicenseCheck repoRoot = do
  let vendorDir = repoRoot </> "vendor"
  exists <- liftIO $ Dir.doesDirectoryExist vendorDir
  if not exists
    then do
      putTextLn $ "Error: vendor directory not found at " <> toText vendorDir
      pure False
    else do
      allEntries <- liftIO $ Dir.listDirectory vendorDir
      validEntries <- flip filterM allEntries $ \e -> do
        let full = vendorDir </> e
        isDir <- liftIO $ Dir.doesDirectoryExist full
        pure (isDir && not ("." `List.isPrefixOf` e))
      let pkgDirs = List.sort (fmap (vendorDir </>) validEntries)
      putTextLn $ "Auditing licenses for " <> show (length pkgDirs) <> " third-party packages in " <> toText vendorDir <> "...\n"
      (failures, successes) <- auditPackages pkgDirs ([], [])
      if not (null failures)
        then do
          putTextLn $ "FAILED: " <> show (length failures) <> " package(s) failed license check:\n"
          for_ failures $ \(name, reason) ->
            putTextLn $ "  ❌ " <> name <> ": " <> reason
          putTextLn "\nApproved licenses:"
          putTextLn $ "  " <> Text.intercalate ", " (Set.toList allowedLicenses)
          pure False
        else do
          putTextLn $ "✅ All " <> show (length successes) <> " third-party packages in vendor/ comply with approved licenses."
          pure True
  where
    auditPackages [] acc = pure acc
    auditPackages (p : ps) (fails, succs) = do
      let name = toText (takeFileName p)
      mLicInfo <- detectPackageLicense p
      case mLicInfo of
        Nothing ->
          auditPackages ps ((name, "Could not determine license") : fails, succs)
        Just (licStr, source) ->
          case parseSpdx licStr of
            Nothing ->
              auditPackages ps ((name, "Failed to parse SPDX expression: " <> licStr <> " (" <> source <> ")") : fails, succs)
            Just spdx ->
              if evalSpdx spdx
                then auditPackages ps (fails, (name, licStr, source) : succs)
                else auditPackages ps ((name, "License " <> licStr <> " (" <> source <> ") is not in approved allowlist") : fails, succs)
