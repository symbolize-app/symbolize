module Dev.Gen
  ( gen,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Vector (Vector)
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.FilePath (FilePath (FilePath))
import Relude (mempty)
import Relude.Applicative (pure)
import Relude.Container (Map, fromList, uncurry)
import Relude.Container.One (one)
import Relude.Foldable (foldMap, forM, forM_)
import Relude.Function (($), (.))
import Relude.Functor (fmap)
import Relude.Monad (Maybe (Just, Nothing), fromMaybe, mapMaybe)
import Relude.Monoid ((<>))
import Relude.String (Text)

gen :: Exec.Exec ()
gen = do
  pnpmWorkspace <- Exec.readFile "pnpm-workspace.yaml" FileFormat.YAML
  pnpmPackages <- forM pnpmWorkspace.packages $ \package -> do
    pnpmPackage <- Exec.readFile (FilePath (package <> "/package.json")) FileFormat.JSON
    pure (package, pnpmPackage)
  rootTaskfileInput <- Exec.readFile "Taskfile.in.yml" FileFormat.YAML

  let pnpmTaskfiles = genPNPMTaskfiles pnpmPackages
  let rootTaskfile = genRootTaskfile pnpmWorkspace rootTaskfileInput

  forM_ pnpmTaskfiles $ \(filePath, pnpmTaskfile) ->
    Exec.writeFile filePath FileFormat.YAML pnpmTaskfile
  Exec.writeFile "Taskfile.yml" FileFormat.YAML rootTaskfile

genPNPMTaskfiles :: Vector (Text, FileFormat.PNPMPackage) -> Vector (FilePath, FileFormat.Taskfile)
genPNPMTaskfiles pnpmPackages =
  uncurry genPNPMTaskfile `fmap` pnpmPackages

genPNPMTaskfile :: Text -> FileFormat.PNPMPackage -> (FilePath, FileFormat.Taskfile)
genPNPMTaskfile package pnpmPackage =
  ( FilePath (package <> "/Taskfile.yml"),
    FileFormat.Taskfile
      { version = FileFormat.taskfileVersion,
        run = FileFormat.taskfileRun,
        includes = Nothing,
        tasks =
          fromList
            [ ( "build",
                FileFormat.TaskfileTask
                  { deps = genPNPMBuildDeps pnpmPackage
                  }
              )
            ]
      }
  )

genPNPMBuildDeps :: FileFormat.PNPMPackage -> Vector Text
genPNPMBuildDeps pnpmPackage =
  fromList . fmap (\package -> ":" <> package <> ":build") $
    genGroup pnpmPackage.dependencies <> genGroup pnpmPackage.devDependencies
  where
    genGroup :: Maybe (Map Text Text) -> [Text]
    genGroup = mapMaybe (Text.stripPrefix "@intertwine/") . foldMap Map.keys

genRootTaskfile :: FileFormat.PNPMWorkspace -> FileFormat.Taskfile -> FileFormat.Taskfile
genRootTaskfile pnpmWorkspace rootTaskfileInput =
  FileFormat.Taskfile
    { version = FileFormat.taskfileVersion,
      run = FileFormat.taskfileRun,
      includes =
        Just
          ( (mempty `fromMaybe` rootTaskfileInput.includes)
              <> ( \package ->
                     one
                       ( package,
                         FileFormat.TaskfileInclude
                           { internal = Nothing,
                             taskfile = package
                           }
                       )
                 )
                `foldMap` pnpmWorkspace.packages
          ),
      tasks =
        rootTaskfileInput.tasks
          <> fromList
            [ ( "pnpm:build",
                FileFormat.TaskfileTask
                  { deps = (<> ":build") `fmap` pnpmWorkspace.packages
                  }
              )
            ]
    }
