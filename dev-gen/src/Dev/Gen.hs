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
import Relude.Applicative (pass, (<*>))
import Relude.Base (Type, (/=), (==))
import Relude.Bool (Bool (False, True), not, otherwise, (&&))
import Relude.Container (fromList, uncurry)
import Relude.Foldable (Foldable, for_, null, toList)
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
    vendorTargets :: Vector (Text, Text),
    gleamPackages :: Vector FileFormat.GleamPackage
  }

type Output :: Type
data Output = Output
  { buckConfig :: Vector Text,
    packageTaskfiles :: Vector (FilePath, FileFormat.Taskfile),
    gleamBuckFiles :: Vector (FilePath, Vector Text),
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
    <*> Exec.async Exec.readGleamPackages

genFiles :: Input -> Output
genFiles i =
  Output
    { buckConfig = genBuckConfig i.buckConfigInput i.gitIgnore,
      packageTaskfiles = genPackageTaskfiles i.workspace,
      gleamBuckFiles = genGleamBuckFiles i.gleamPackages,
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
    *> asyncWriteAll Exec.writeLines o.gleamBuckFiles
    *> Exec.async (Exec.writeLines "Procfile" o.procfile)
    *> Exec.async (Exec.writeYAML "Taskfile.yml" o.rootTaskfile)
    *> Exec.async (Exec.writeLines ".sqlfluffignore" o.sqlFluffIgnore)
    *> Exec.async (Exec.writeLines "vendor/BUCK" o.vendorBuck)
    *> Exec.async (Exec.writeJSON ".watchmanconfig" o.watchmanConfig)

genGleamBuckFiles ::
  Vector FileFormat.GleamPackage ->
  Vector (FilePath, Vector Text)
genGleamBuckFiles pkgs =
  genGleamBuckFile <$> pkgs

genGleamBuckFile :: FileFormat.GleamPackage -> (FilePath, Vector Text)
genGleamBuckFile pkg =
  ( FilePath (pkg.dir <> "/BUCK"),
    genGleamBuck pkg
  )

genGleamBuck :: FileFormat.GleamPackage -> Vector Text
genGleamBuck pkg =
  let header =
        [ "load(\"@dev_buck//:gleam.bzl\", \"gleam_package\")",
          "",
          "gleam_package(",
          "    name = \"" <> pkg.dir <> "\",",
          "    package_name = \"" <> pkg.name <> "\","
        ]
      depsSection =
        if null pkg.deps
          then []
          else
            ["    deps = ["]
              <> ["        \"" <> d <> "\"," | d <- toList pkg.deps]
              <> ["    ],"]
      footer =
        [ ")",
          ""
        ]
      extra
        | pkg.dir == "svc-gateway-guest-run" =
            [ "load(\"@dev_buck//:esbuild.bzl\", \"esbuild_manifest\")",
              "",
              "esbuild_manifest(",
              "    name = \"manifest\",",
              "    mode = \"development\",",
              ")",
              "",
              "esbuild_manifest(",
              "    name = \"manifest-release\",",
              "    mode = \"production\",",
              ")",
              ""
            ]
        | pkg.dir == "dev-esbuild" =
            [ "load(\"@prelude//rules.bzl\", \"export_file\")",
              "",
              "[",
              "    export_file(",
              "        name = f,",
              "        src = f,",
              "        visibility = [\"PUBLIC\"],",
              "    )",
              "    for f in native.glob([\"query/*.sql\"])",
              "]",
              ""
            ]
        | otherwise = []
   in fromList (header <> depsSection <> footer <> extra)

genPackageTaskfiles ::
  FileFormat.Workspace ->
  Vector (FilePath, FileFormat.Taskfile)
genPackageTaskfiles workspace =
  genRustPackageTaskfile <$> workspace.rustMembers

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
            uncurry (genRustTask rustPackageName)
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

genRustTask :: Text -> Text -> Vector Text -> (Text, FileFormat.TaskfileTask)
genRustTask pkgName taskName aliases =
  ( taskName,
    FileFormat.TaskfileTask
      { aliases = Just aliases,
        deps = Nothing,
        cmd = Nothing,
        cmds = Just (Vector.singleton (commandFor taskName))
      }
  )
  where
    commandFor "run:debug" = "buck2 run -m debug //" <> pkgName <> " -- {{.CLI_ARGS}}"
    commandFor "run:debug:watch" = "buck2 run -m {{.TASK_WATCHMAN_CLIENT_MODE}} //dev-watchman-client -- --target //" <> pkgName <> " --restart -- {{.CLI_ARGS}}"
    commandFor "run:release" = "buck2 run -m release //" <> pkgName <> " -- {{.CLI_ARGS}}"
    commandFor "test:debug" = "buck2 test -m debug //" <> pkgName <> ":test"
    commandFor "test:debug:watch" = "buck2 run -m {{.TASK_WATCHMAN_CLIENT_MODE}} //dev-watchman-client -- --target //" <> pkgName <> ":test"
    commandFor "test:release" = "buck2 test -m release //" <> pkgName <> ":test"
    commandFor _ = "true"

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
                member
                  <> "__test: buck2 run -m $TASK_WATCHMAN_CLIENT_MODE //dev-watchman-client -- --target //"
                  <> member
                  <> ":test",
              whenTrue
                (Package.isRustService member)
                $ member
                  <> "__run: buck2 run -m $TASK_WATCHMAN_CLIENT_MODE //dev-watchman-client -- --target //"
                  <> member
                  <> " --restart"
            ]
      )
      workspace.rustMembers

genRootTaskfile ::
  FileFormat.Workspace ->
  FileFormat.Taskfile ->
  FileFormat.Taskfile
genRootTaskfile workspace rootTaskfileInput =
  let rustPackageNames = workspace.rustMembers
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
                deps = Nothing,
                cmd = Nothing,
                cmds = Just (Vector.singleton "buck2 test -m debug $(buck2 uquery \"kind('rust_test', //...)\")")
              }
          ),
          ( "rust:test:release",
            FileFormat.TaskfileTask
              { aliases = Just ["rust:tr"],
                deps = Nothing,
                cmd = Nothing,
                cmds = Just (Vector.singleton "buck2 test -m release $(buck2 uquery \"kind('rust_test', //...)\")")
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
