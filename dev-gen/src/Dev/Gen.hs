module Dev.Gen
  ( gen,
  )
where

import Control.Applicative ((*>))
import Data.Foldable (foldMap')
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
import Relude.Container (fromList, uncurry)
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
  { packageTaskfiles :: Vector (FilePath, FileFormat.Taskfile),
    procfile :: Vector Text,
    rootTaskfile :: FileFormat.Taskfile,
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
    { packageTaskfiles = genPackageTaskfiles i.cargoWorkspace,
      procfile = genProcfile i.cargoWorkspace i.procfileInput,
      rootTaskfile =
        genRootTaskfile
          i.cargoWorkspace
          pnpmPackages
          i.rootTaskfileInput,
      sqlFluffIgnore = i.gitIgnore,
      watchmanConfig = genWatchmanConfig i.gitIgnore
    }
  where
    pnpmPackages = Package.transformPNPM i.pnpmPackageFiles

writeFiles :: Output -> Exec.ExecConcurrently ()
writeFiles o =
  pass
    *> asyncWriteAll Exec.writeYAML o.packageTaskfiles
    *> Exec.async (Exec.writeLines "Procfile" o.procfile)
    *> Exec.async (Exec.writeYAML "Taskfile.yml" o.rootTaskfile)
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

genPackageTaskfiles ::
  FileFormat.CargoWorkspace ->
  Vector (FilePath, FileFormat.Taskfile)
genPackageTaskfiles cargoWorkspace =
  genCargoPackageTaskfile <$> cargoWorkspace.workspace.members

genCargoPackageTaskfile :: Text -> (FilePath, FileFormat.Taskfile)
genCargoPackageTaskfile cargoPackageName =
  ( FilePath (cargoPackageName <> "/Taskfile.yml"),
    FileFormat.Taskfile
      { version = FileFormat.taskfileVersion,
        run = FileFormat.taskfileRun,
        includes = Nothing,
        vars = Just [("NAME", cargoPackageName)],
        tasks =
          fromList . toList $
            uncurry genCargoTask
              <$> Vector.catMaybes
                [ whenService ("run:debug", ["run", "r"]),
                  whenService ("run:debug:watch", ["run:watch", "rw"]),
                  whenService ("run:release", ["rr"]),
                  Just ("test:debug", ["test", "t"]),
                  Just ("test:debug:watch", ["test:watch", "tw"]),
                  Just ("test:release", ["tr"])
                ]
      }
  )
  where
    whenService = whenTrue $ Package.isCargoService cargoPackageName

genCargoTask :: Text -> Vector Text -> (Text, FileFormat.TaskfileTask)
genCargoTask name aliases =
  ( name,
    FileFormat.TaskfileTask
      { aliases = Just aliases,
        deps = Nothing,
        cmd =
          Just
            ( FileFormat.TaskfileCommand
                { task = ":cargo:execute-package:" <> name,
                  vars = Just [("NAME", "{{.NAME}}")]
                }
            ),
        cmds = Nothing
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
                (Package.isCargoService member)
                $ member <> "__run: task " <> member <> ":run:watch"
            ]
      )
      cargoWorkspace.workspace.members

genRootTaskfile ::
  FileFormat.CargoWorkspace ->
  Vector Package.PNPM ->
  FileFormat.Taskfile ->
  FileFormat.Taskfile
genRootTaskfile cargoWorkspace pnpmPackages rootTaskfileInput =
  let cargoPackageNames = cargoWorkspace.workspace.members
      pnpmPackageNames = (.name) <$> pnpmPackages
      newIncludes =
        fromList
          . toList
          $ ( \name ->
                ( name,
                  FileFormat.TaskfileInclude
                    { internal = Nothing,
                      taskfile = name
                    }
                )
            )
            <$> (cargoPackageNames <> pnpmPackageNames)
      newTasks =
        [ ( "cargo:test:debug",
            FileFormat.TaskfileTask
              { aliases = Just ["cargo:test", "cargo:t"],
                deps =
                  Just
                    ( (<> ":test:debug") <$> cargoPackageNames
                    ),
                cmd = Nothing,
                cmds = Nothing
              }
          ),
          ( "cargo:test:release",
            FileFormat.TaskfileTask
              { aliases = Just ["cargo:tr"],
                deps =
                  Just
                    ( (<> ":test:release") <$> cargoPackageNames
                    ),
                cmd = Nothing,
                cmds = Nothing
              }
          ),
          ( "pnpm:link-build-dirs",
            FileFormat.TaskfileTask
              { aliases = Nothing,
                deps =
                  Just
                    ( (<> ":link-build-dir") <$> pnpmPackageNames
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
