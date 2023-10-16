module Dev.Gen
  ( gen,
  )
where

import Data.Vector (Vector)
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.FilePath (FilePath (FilePath))
import Relude (mempty)
import Relude.Container (fromList)
import Relude.Container.One (one)
import Relude.Foldable (foldMap, forM_)
import Relude.Function (($))
import Relude.Functor (fmap)
import Relude.Monad (Maybe (Just, Nothing), fromMaybe)
import Relude.Monoid ((<>))
import Relude.String (Text)

gen :: Exec.Exec ()
gen = do
  pnpmWorkspace <- Exec.readFile "pnpm-workspace.yaml" FileFormat.YAML
  rootTaskfileInput <- Exec.readFile "Taskfile.in.yml" FileFormat.YAML

  let pnpmTaskfiles = genPNPMTaskfiles pnpmWorkspace
  let rootTaskfile = genRootTaskfile pnpmWorkspace rootTaskfileInput

  forM_ pnpmTaskfiles $ \(filePath, pnpmTaskfile) ->
    Exec.writeFile filePath FileFormat.YAML pnpmTaskfile
  Exec.writeFile "Taskfile.yml" FileFormat.YAML rootTaskfile

genPNPMTaskfiles :: FileFormat.PNPMWorkspace -> Vector (FilePath, FileFormat.Taskfile)
genPNPMTaskfiles pnpmWorkspace =
  genPNPMTaskfile `fmap` pnpmWorkspace.packages

genPNPMTaskfile :: Text -> (FilePath, FileFormat.Taskfile)
genPNPMTaskfile package =
  ( FilePath (package <> "/Taskfile.yml"),
    FileFormat.Taskfile
      { version = FileFormat.taskfileVersion,
        run = FileFormat.taskfileRun,
        includes = Nothing,
        tasks = fromList []
      }
  )

genRootTaskfile :: FileFormat.PNPMWorkspace -> FileFormat.Taskfile -> FileFormat.Taskfile
genRootTaskfile pnpmWorkspace rootTaskfileInput =
  FileFormat.Taskfile
    { version = FileFormat.taskfileVersion,
      run = FileFormat.taskfileRun,
      includes =
        Just
          ( (mempty `fromMaybe` rootTaskfileInput.includes)
              <> ( \package ->
                     one (package, FileFormat.TaskfileInclude {internal = Nothing, taskfile = package})
                 )
                `foldMap` pnpmWorkspace.packages
          ),
      tasks = rootTaskfileInput.tasks
    }
