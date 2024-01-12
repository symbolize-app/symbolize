module Dev.Gen
  ( gen,
  )
where

import Control.Applicative ((*>))
import Data.Foldable (foldMap')
import Data.Text qualified as Text
import Data.Traversable (for)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.FilePath (FilePath (FilePath))
import Dev.Gen.Package qualified as Package
import Relude.Applicative (pass, pure, (<*>))
import Relude.Base (Type)
import Relude.Bool (Bool (False, True))
import Relude.Container (fromList)
import Relude.Foldable (Foldable, for_, toList)
import Relude.Function (const, ($), (.))
import Relude.Functor ((<$>))
import Relude.Monad (Maybe (Just, Nothing), (=<<))
import Relude.Monoid (maybeToMonoid, (<>))
import Relude.String (Text)

type Input :: Type
data Input = Input
  { cargoWorkspace :: FileFormat.CargoWorkspace,
    gitIgnore :: Vector Text,
    pnpmPackageFiles :: Vector (Text, FileFormat.PNPMPackageFile),
    procfileInput :: Vector Text,
    rootTaskfileInput :: FileFormat.Taskfile
  }

type Output :: Type
data Output = Output
  { esLintConfig :: FileFormat.ESLintConfig,
    packageTaskfiles :: Vector (FilePath, FileFormat.Taskfile),
    packageTypeScriptConfigs :: Vector (FilePath, FileFormat.TypeScriptConfig),
    procfile :: Vector Text,
    rootTaskfile :: FileFormat.Taskfile,
    rootTypeScriptConfig :: FileFormat.TypeScriptConfig,
    sqlFluffIgnore :: Vector Text,
    watchmanConfig :: FileFormat.WatchmanConfig
  }

gen :: Exec.Exec ()
gen = Exec.await . writeFiles . genFiles =<< Exec.await asyncReadFiles

asyncReadFiles :: Exec.ExecConcurrently Input
asyncReadFiles =
  Input
    <$> Exec.async (Exec.readTOML "Cargo.toml")
    <*> Exec.async (Exec.readLines ".gitignore")
    <*> Exec.async readPNPMPackageFiles
    <*> Exec.async (Exec.readLines "Procfile.in")
    <*> Exec.async (Exec.readYAML "Taskfile.in.yml")

genFiles :: Input -> Output
genFiles i =
  Output
    { esLintConfig = genESLintConfig pnpmPackages,
      packageTaskfiles = genPackageTaskfiles pnpmPackages,
      packageTypeScriptConfigs = genPackageTypeScriptConfigs pnpmPackages,
      procfile = genProcfile i.cargoWorkspace i.procfileInput,
      rootTaskfile = genRootTaskfile pnpmPackages i.rootTaskfileInput,
      rootTypeScriptConfig = genRootTypeScriptConfig pnpmPackages,
      sqlFluffIgnore = i.gitIgnore,
      watchmanConfig = genWatchmanConfig i.gitIgnore
    }
  where
    pnpmPackages = Package.transformPNPM i.pnpmPackageFiles

writeFiles :: Output -> Exec.ExecConcurrently ()
writeFiles o =
  pass
    *> Exec.async (Exec.writeJSON ".eslintrc.json" o.esLintConfig)
    *> asyncWriteAll Exec.writeYAML o.packageTaskfiles
    *> asyncWriteAll Exec.writeJSON o.packageTypeScriptConfigs
    *> Exec.async (Exec.writeLines "Procfile" o.procfile)
    *> Exec.async (Exec.writeYAML "Taskfile.yml" o.rootTaskfile)
    *> Exec.async (Exec.writeJSON "tsconfig.json" o.rootTypeScriptConfig)
    *> Exec.async (Exec.writeLines ".sqlfluffignore" o.sqlFluffIgnore)
    *> Exec.async (Exec.writeJSON ".watchmanconfig" o.watchmanConfig)

readPNPMPackageFiles ::
  Exec.Exec (Vector (Text, FileFormat.PNPMPackageFile))
readPNPMPackageFiles = do
  pnpmWorkspace <-
    Exec.readYAML "pnpm-workspace.yaml" ::
      Exec.Exec FileFormat.PNPMWorkspace
  Exec.await $
    for
      pnpmWorkspace.packages
      ( \package ->
          (package,)
            <$> Exec.async
              ( Exec.readJSON
                  ( FilePath (package <> "/package.json")
                  )
              )
      )

genESLintConfig :: Vector Package.PNPM -> FileFormat.ESLintConfig
genESLintConfig pnpmPackages =
  FileFormat.ESLintConfig
    { extends = FileFormat.esLintConfigExtends,
      parserOptions =
        FileFormat.ESLintConfigParserOptions
          { tsconfigRootDir =
              FileFormat.esLintConfigParserOptionsTsconfigRootDir,
            project =
              (<> "/tsconfig.json")
                <$> Package.foldTypeScriptPackageNames pnpmPackages
          }
    }

genPackageTaskfiles ::
  Vector Package.PNPM -> Vector (FilePath, FileFormat.Taskfile)
genPackageTaskfiles pnpmPackages = genPNPMPackageTaskfile <$> pnpmPackages

genPNPMPackageTaskfile :: Package.PNPM -> (FilePath, FileFormat.Taskfile)
genPNPMPackageTaskfile pnpmPackage =
  ( FilePath (pnpmPackage.name <> "/Taskfile.yml"),
    FileFormat.Taskfile
      { version = FileFormat.taskfileVersion,
        run = FileFormat.taskfileRun,
        includes = Nothing,
        vars = Just [("NAME", pnpmPackage.name)],
        tasks =
          [ ( "link-build-dir",
              FileFormat.TaskfileTask
                { aliases = Nothing,
                  deps = Nothing,
                  cmd =
                    Just
                      ( FileFormat.TaskfileCommand
                          { task = ":tmpfs:link-package-build-dir",
                            vars = Just [("NAME", "{{.NAME}}")]
                          }
                      ),
                  cmds = Nothing
                }
            )
          ]
      }
  )

genPackageTypeScriptConfigs ::
  Vector Package.PNPM -> Vector (FilePath, FileFormat.TypeScriptConfig)
genPackageTypeScriptConfigs = Vector.mapMaybe genPackageTypeScriptConfig

genPackageTypeScriptConfig ::
  Package.PNPM ->
  Maybe (FilePath, FileFormat.TypeScriptConfig)
genPackageTypeScriptConfig pnpmPackage = do
  typeScript <- pnpmPackage.typeScript
  pure
    ( FilePath (pnpmPackage.name <> "/tsconfig.json"),
      FileFormat.TypeScriptConfig
        { extends = FileFormat.typeScriptConfigExtends,
          include = FileFormat.typeScriptConfigInclude,
          exclude = FileFormat.typeScriptConfigExclude,
          compilerOptions = FileFormat.typeScriptConfigCompilerOptions,
          references =
            FileFormat.TypeScriptConfigReference . ("../" <>)
              <$> typeScript.dependencies
        }
    )

genProcfile ::
  FileFormat.CargoWorkspace ->
  Vector Text ->
  Vector Text
genProcfile cargoWorkspace procfileInput =
  procfileInput
    <> foldMap'
      ( \member ->
          Vector.catMaybes
            [ Just $
                member <> "__test: task " <> member <> ":test:watch",
              whenTrue
                (Text.isPrefixOf "svc-" member)
                $ member <> "__run: task " <> member <> ":run:watch"
            ]
      )
      cargoWorkspace.workspace.members

genRootTaskfile ::
  Vector Package.PNPM ->
  FileFormat.Taskfile ->
  FileFormat.Taskfile
genRootTaskfile pnpmPackages rootTaskfileInput =
  let newIncludes =
        fromList
          . toList
          $ ( \pnpmPackage ->
                ( pnpmPackage.name,
                  FileFormat.TaskfileInclude
                    { internal = Nothing,
                      taskfile = pnpmPackage.name
                    }
                )
            )
            <$> pnpmPackages
      newTasks =
        [ ( "pnpm:link-build-dirs",
            FileFormat.TaskfileTask
              { aliases = Nothing,
                deps =
                  Just
                    ( (<> ":link-build-dir") . (.name) <$> pnpmPackages
                    ),
                cmd = Nothing,
                cmds = Nothing
              }
          )
        ]
   in FileFormat.Taskfile
        { version = FileFormat.taskfileVersion,
          run = FileFormat.taskfileRun,
          includes =
            Just (maybeToMonoid rootTaskfileInput.includes <> newIncludes),
          vars = rootTaskfileInput.vars,
          tasks = rootTaskfileInput.tasks <> newTasks
        }

genRootTypeScriptConfig ::
  Vector Package.PNPM ->
  FileFormat.TypeScriptConfig
genRootTypeScriptConfig pnpmPackages =
  FileFormat.TypeScriptConfig
    { extends = FileFormat.typeScriptConfigExtends,
      include = [],
      exclude = [],
      compilerOptions = FileFormat.typeScriptConfigCompilerOptions,
      references =
        FileFormat.TypeScriptConfigReference . ("./" <>)
          <$> Package.foldTypeScriptPackageNames pnpmPackages
    }

genWatchmanConfig :: Vector Text -> FileFormat.WatchmanConfig
genWatchmanConfig gitIgnore =
  FileFormat.WatchmanConfig
    { ignoreDirs = gitIgnore
    }

asyncWriteAll ::
  (Foldable t) =>
  (FilePath -> a -> Exec.Exec ()) ->
  t (FilePath, a) ->
  Exec.ExecConcurrently ()
asyncWriteAll write filePairs =
  for_
    filePairs
    ( \(filePath, file) ->
        Exec.async (write filePath file)
    )

whenTrue :: Bool -> a -> Maybe a
whenTrue True = Just
whenTrue False = const Nothing
