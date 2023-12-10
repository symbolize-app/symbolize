module Dev.Gen
  ( gen,
  )
where

import Control.Applicative ((*>))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.FilePath (FilePath (FilePath))
import Dev.Gen.Package qualified as Package
import Relude.Applicative (pass, pure)
import Relude.Container (fromList)
import Relude.Foldable (for_, toList, traverse)
import Relude.Function (($), (.))
import Relude.Functor ((<$>))
import Relude.Monad (Maybe (Just, Nothing))
import Relude.Monoid (maybeToMonoid, (<>))

gen :: Exec.Exec ()
gen = do
  (gitIgnore, rootTaskfileInput, pnpmPackageFiles) <-
    Exec.await3
      ( Exec.async $
          Exec.readLines ".gitignore"
      )
      ( Exec.async $
          Exec.readYAML "Taskfile.in.yml"
      )
      ( Exec.async $ do
          pnpmWorkspace <-
            Exec.readYAML "pnpm-workspace.yaml" ::
              Exec.Exec FileFormat.PNPMWorkspace
          Exec.await
            ( traverse
                ( \package ->
                    (package,)
                      <$> Exec.async
                        ( Exec.readJSON
                            ( FilePath (package <> "/package.json")
                            )
                        )
                )
                pnpmWorkspace.packages
            )
      )

  let pnpmPackages =
        Package.transformPNPM (fromList (toList pnpmPackageFiles))
  let packageTypeScriptConfigs =
        Vector.mapMaybe genPackageTypeScriptConfig pnpmPackages
  let rootTypeScriptConfig = genRootTypeScriptConfig pnpmPackages
  let esLintConfig = genESLintConfig pnpmPackages
  let packageTaskfiles = genPNPMTaskfile <$> pnpmPackages
  let rootTaskfile = genRootTaskfile pnpmPackages rootTaskfileInput

  Exec.await_ $
    pass
      *> Exec.async (Exec.writeLines ".sqlfluffignore" gitIgnore)
      *> for_
        packageTypeScriptConfigs
        ( \(filePath, packageTypeScriptConfig) ->
            Exec.async (Exec.writeJSON filePath packageTypeScriptConfig)
        )
      *> Exec.async (Exec.writeJSON "tsconfig.json" rootTypeScriptConfig)
      *> Exec.async (Exec.writeJSON ".eslintrc.json" esLintConfig)
      *> for_
        packageTaskfiles
        ( \(filePath, packageTaskfile) ->
            Exec.async (Exec.writeYAML filePath packageTaskfile)
        )
      *> Exec.async (Exec.writeYAML "Taskfile.yml" rootTaskfile)

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

genPNPMTaskfile :: Package.PNPM -> (FilePath, FileFormat.Taskfile)
genPNPMTaskfile pnpmPackage =
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
