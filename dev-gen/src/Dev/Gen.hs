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
import Relude.Bool (Bool (False, True))
import Relude.Container (fromList)
import Relude.Foldable (Foldable, for_, toList)
import Relude.Function (const, ($), (.))
import Relude.Functor ((<$>))
import Relude.Monad (Maybe (Just, Nothing))
import Relude.Monoid (maybeToMonoid, (<>))
import Relude.String (Text)

gen :: Exec.Exec ()
gen = do
  ( gitIgnore,
    rootTaskfileInput,
    pnpmPackageFiles,
    cargoWorkspace,
    procfileInput
    ) <-
    Exec.await $
      (,,,,)
        <$> Exec.async (Exec.readLines ".gitignore")
        <*> Exec.async (Exec.readYAML "Taskfile.in.yml")
        <*> Exec.async readPNPMPackageFiles
        <*> Exec.async (Exec.readTOML "Cargo.toml")
        <*> Exec.async (Exec.readLines "Procfile.in")

  let pnpmPackages = Package.transformPNPM pnpmPackageFiles
  let packageTypeScriptConfigs = genPackageTypeScriptConfigs pnpmPackages
  let rootTypeScriptConfig = genRootTypeScriptConfig pnpmPackages
  let esLintConfig = genESLintConfig pnpmPackages
  let packageTaskfiles = genPackageTaskfiles pnpmPackages
  let rootTaskfile = genRootTaskfile pnpmPackages rootTaskfileInput
  let procfile = genProcfile cargoWorkspace procfileInput

  Exec.await_ $
    pass
      *> Exec.async (Exec.writeLines ".sqlfluffignore" gitIgnore)
      *> asyncWriteAll Exec.writeJSON packageTypeScriptConfigs
      *> Exec.async (Exec.writeJSON "tsconfig.json" rootTypeScriptConfig)
      *> Exec.async (Exec.writeJSON ".eslintrc.json" esLintConfig)
      *> asyncWriteAll Exec.writeYAML packageTaskfiles
      *> Exec.async (Exec.writeYAML "Taskfile.yml" rootTaskfile)
      *> Exec.async (Exec.writeLines "Procfile" procfile)

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
