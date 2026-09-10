module Dev.Gen
  ( gen,
  )
where

import Control.Applicative ((*>))
import Data.Foldable (foldMap')
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.FilePath (FilePath (FilePath))
import Dev.Gen.Package qualified as Package
import Dev.Gen.Vendor.Target qualified as Target
import Relude.Applicative (pass, pure, (<*>))
import Relude.Base (Type, (/=), (==))
import Relude.Bool (Bool (False, True), not, (&&), (||))
import Relude.Container (fromList, uncurry)
import Relude.Foldable (Foldable, for_, toList)
import Relude.Function (const, ($), (.))
import Relude.Functor ((<$>))
import Relude.Monad (Maybe (Just, Nothing), fromMaybe, (=<<))
import Relude.Monoid (maybeToMonoid, (<>))
import Relude.Numeric ((-))
import Relude.String (Text)

type Input :: Type
data Input = Input
  { buckConfigInput :: Vector Text,
    workspace :: FileFormat.Workspace,
    gitIgnore :: Vector Text,
    procfileInput :: Vector Text,
    rootTaskfileInput :: FileFormat.Taskfile,
    vendorTargets :: Vector (Text, Text)
  }

type Output :: Type
data Output = Output
  { buckConfig :: Vector Text,
    packageTaskfiles :: Vector (FilePath, FileFormat.Taskfile),
    procfile :: Vector Text,
    rootTaskfile :: FileFormat.Taskfile,
    sqlFluffIgnore :: Vector Text,
    vendorBuck :: Vector Text,
    watchmanConfig :: FileFormat.WatchmanConfig
  }

gen :: Exec.Exec ()
gen = Exec.await . writeFiles . genFiles =<< Exec.await asyncReadFiles

asyncReadFiles :: Exec.ExecConcurrently Input
asyncReadFiles =
  Input
    <$> Exec.async (Exec.readLines ".buckconfig.in")
    <*> Exec.async (Exec.readTOML "workspace.bzl")
    <*> Exec.async (Exec.readLines ".gitignore")
    <*> Exec.async (Exec.readLines "Procfile.in")
    <*> Exec.async (Exec.readYAML "Taskfile.in.yml")
    <*> Exec.async Exec.readVendorTargets

genFiles :: Input -> Output
genFiles i =
  Output
    { buckConfig = genBuckConfig i.buckConfigInput i.gitIgnore,
      packageTaskfiles = genPackageTaskfiles i.workspace,
      procfile = genProcfile i.workspace i.procfileInput,
      rootTaskfile =
        genRootTaskfile
          i.workspace
          i.rootTaskfileInput,
      sqlFluffIgnore = genSqlFluffIgnore i.gitIgnore,
      vendorBuck = Target.genVendorBuck i.vendorTargets,
      watchmanConfig = genWatchmanConfig i.gitIgnore
    }

writeFiles :: Output -> Exec.ExecConcurrently ()
writeFiles o =
  pass
    *> Exec.async (Exec.writeLines ".buckconfig" o.buckConfig)
    *> asyncWriteAll Exec.writeYAML o.packageTaskfiles
    *> Exec.async (Exec.writeLines "Procfile" o.procfile)
    *> Exec.async (Exec.writeYAML "Taskfile.yml" o.rootTaskfile)
    *> Exec.async (Exec.writeLines ".sqlfluffignore" o.sqlFluffIgnore)
    *> Exec.async (Exec.writeLines "vendor/BUCK" o.vendorBuck)
    *> Exec.async (Exec.writeJSON ".watchmanconfig" o.watchmanConfig)

genPackageTaskfiles ::
  FileFormat.Workspace ->
  Vector (FilePath, FileFormat.Taskfile)
genPackageTaskfiles workspace =
  genRustPackageTaskfile <$> workspace.members

genRustPackageTaskfile :: Text -> (FilePath, FileFormat.Taskfile)
genRustPackageTaskfile rustPackageName =
  ( FilePath (rustPackageName <> "/Taskfile.yml"),
    FileFormat.Taskfile
      { version = FileFormat.taskfileVersion,
        run = FileFormat.taskfileRun,
        includes = Nothing,
        vars = Just [("NAME", rustPackageName)],
        tasks =
          fromList . toList $
            uncurry genRustTask
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
    whenService = whenTrue $ Package.isRustService rustPackageName

genRustTask :: Text -> Vector Text -> (Text, FileFormat.TaskfileTask)
genRustTask name aliases =
  ( name,
    FileFormat.TaskfileTask
      { aliases = Just aliases,
        deps = Nothing,
        cmd =
          Just
            ( FileFormat.TaskfileCommand
                { task = ":rust:execute-package:" <> name,
                  vars = Just [("NAME", "{{.NAME}}")]
                }
            ),
        cmds = Nothing
      }
  )

genProcfile ::
  FileFormat.Workspace ->
  Vector Text ->
  Vector Text
genProcfile workspace procfileInput =
  procfileInput
    <> foldMap'
      ( \member ->
          Vector.catMaybes
            [ Just $
                member <> "__test: task " <> member <> ":test:watch",
              whenTrue
                (Package.isRustService member)
                $ member <> "__run: task " <> member <> ":run:watch"
            ]
      )
      workspace.members

genRootTaskfile ::
  FileFormat.Workspace ->
  FileFormat.Taskfile ->
  FileFormat.Taskfile
genRootTaskfile workspace rootTaskfileInput =
  let rustPackageNames = workspace.members
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
            <$> rustPackageNames
      newTasks =
        [ ( "rust:test:debug",
            FileFormat.TaskfileTask
              { aliases = Just ["rust:test", "rust:t"],
                deps =
                  Just
                    ( (<> ":test:debug") <$> rustPackageNames
                    ),
                cmd = Nothing,
                cmds = Nothing
              }
          ),
          ( "rust:test:release",
            FileFormat.TaskfileTask
              { aliases = Just ["rust:tr"],
                deps =
                  Just
                    ( (<> ":test:release") <$> rustPackageNames
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

genSqlFluffIgnore :: Vector Text -> Vector Text
genSqlFluffIgnore gitIgnore = Vector.snoc gitIgnore "vendor"

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

genBuckConfig :: Vector Text -> Vector Text -> Vector Text
genBuckConfig buckConfigInput gitIgnore =
  buckConfigInput
    <> [ "",
         "[project]",
         "  ignore = \\"
       ]
    <> genIgnores
  where
    normalize t = fromMaybe t (Text.stripPrefix "/" t)
    cleanGit =
      Vector.cons
        ".git"
        ( Vector.map
            normalize
            ( Vector.filter
                (\t -> not (Text.null t) && not (Text.isPrefixOf "#" t) && t /= ".git" && t /= "/.git")
                gitIgnore
            )
        )
    len = Vector.length cleanGit
    genIgnores =
      Vector.imap
        ( \i item ->
            "    " <> item <> if i == (len - 1) then "" else ", \\"
        )
        cleanGit
