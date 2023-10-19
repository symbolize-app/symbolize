module Dev.Gen
  ( gen,
  )
where

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.FilePath (FilePath (FilePath))
import Dev.Gen.Package qualified as Package
import Relude.Applicative (pure)
import Relude.Container (fromList)
import Relude.Foldable (forM, forM_, toList)
import Relude.Function (($), (.))
import Relude.Functor (fmap)
import Relude.Monad (Maybe (Just, Nothing), isJust)
import Relude.Monoid (maybeToMonoid, (<>))

gen :: Exec.Exec ()
gen = do
  pnpmWorkspace <- Exec.readFile "pnpm-workspace.yaml" FileFormat.YAML :: Exec.Exec FileFormat.PNPMWorkspace
  pnpmPackageFiles <- forM pnpmWorkspace.packages $ \package -> do
    pnpmPackageFile <- Exec.readFile (FilePath (package <> "/package.json")) FileFormat.JSON
    pure (package, pnpmPackageFile)
  rootTaskfileInput <- Exec.readFile "Taskfile.in.yml" FileFormat.YAML

  let pnpmPackages = Package.transformPNPM (fromList (toList pnpmPackageFiles))
  let packageTypeScriptConfigs = Vector.mapMaybe genPackageTypeScriptConfig pnpmPackages
  let rootTypeScriptConfig = genRootTypeScriptConfig pnpmPackages
  let esLintConfig = genESLintConfig pnpmPackages
  let packageTaskfiles = fmap genPNPMTaskfile pnpmPackages
  let rootTaskfile = genRootTaskfile pnpmPackages rootTaskfileInput

  forM_ packageTypeScriptConfigs $ \(filePath, packageTypeScriptConfig) ->
    Exec.writeFile filePath FileFormat.JSON packageTypeScriptConfig
  Exec.writeFile "tsconfig.json" FileFormat.JSON rootTypeScriptConfig
  Exec.writeFile ".eslintrc.json" FileFormat.JSON esLintConfig
  forM_ packageTaskfiles $ \(filePath, packageTaskfile) ->
    Exec.writeFile filePath FileFormat.YAML packageTaskfile
  Exec.writeFile "Taskfile.yml" FileFormat.YAML rootTaskfile

genPackageTypeScriptConfig :: Package.PNPM -> Maybe (FilePath, FileFormat.TypeScriptConfig)
genPackageTypeScriptConfig pnpmPackage = do
  typeScript <- pnpmPackage.typeScript
  pure
    ( FilePath (pnpmPackage.name <> "/tsconfig.json"),
      FileFormat.TypeScriptConfig
        { extends = FileFormat.typeScriptConfigExtends,
          include = FileFormat.typeScriptConfigInclude,
          exclude = FileFormat.typeScriptConfigExclude,
          compilerOptions = FileFormat.typeScriptConfigCompilerOptions,
          references = fmap (FileFormat.TypeScriptConfigReference . ("../" <>)) typeScript.dependencies
        }
    )

genRootTypeScriptConfig :: Vector Package.PNPM -> FileFormat.TypeScriptConfig
genRootTypeScriptConfig pnpmPackages =
  FileFormat.TypeScriptConfig
    { extends = FileFormat.typeScriptConfigExtends,
      include = [],
      exclude = [],
      compilerOptions = FileFormat.typeScriptConfigCompilerOptions,
      references =
        fmap (FileFormat.TypeScriptConfigReference . ("./" <>))
          . Package.foldTypeScriptPackageNames
          $ pnpmPackages
    }

genESLintConfig :: Vector Package.PNPM -> FileFormat.ESLintConfig
genESLintConfig pnpmPackages =
  FileFormat.ESLintConfig
    { extends = FileFormat.esLintConfigExtends,
      parserOptions =
        FileFormat.ESLintConfigParserOptions
          { tsconfigRootDir = FileFormat.esLintConfigParserOptionsTsconfigRootDir,
            project =
              fmap (<> "/tsconfig.json")
                . Package.foldTypeScriptPackageNames
                $ pnpmPackages
          }
    }

genPNPMTaskfile :: Package.PNPM -> (FilePath, FileFormat.Taskfile)
genPNPMTaskfile pnpmPackage =
  ( FilePath (pnpmPackage.name <> "/Taskfile.yml"),
    FileFormat.Taskfile
      { version = FileFormat.taskfileVersion,
        run = FileFormat.taskfileRun,
        includes = Nothing,
        vars = Nothing,
        tasks = []
      }
  )

genRootTaskfile :: Vector Package.PNPM -> FileFormat.Taskfile -> FileFormat.Taskfile
genRootTaskfile pnpmPackages rootTaskfileInput =
  let newIncludes =
        fromList
          . toList
          . fmap
            ( \pnpmPackage ->
                ( pnpmPackage.name,
                  FileFormat.TaskfileInclude
                    { internal = Nothing,
                      taskfile = pnpmPackage.name
                    }
                )
            )
          $ pnpmPackages
      newVars =
        [ ( "PNPM_TYPESCRIPT_PACKAGES",
            Text.intercalate " " . toList $ Package.foldTypeScriptPackageNames pnpmPackages
          )
        ]
   in FileFormat.Taskfile
        { version = FileFormat.taskfileVersion,
          run = FileFormat.taskfileRun,
          includes =
            Just (maybeToMonoid rootTaskfileInput.includes <> newIncludes),
          vars =
            Just (maybeToMonoid rootTaskfileInput.vars <> newVars),
          tasks = rootTaskfileInput.tasks
        }
