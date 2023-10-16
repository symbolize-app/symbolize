module Dev.Gen
  ( gen,
  )
where

import Dev.Gen.Exec qualified as Exec
import Dev.Gen.FileFormat qualified as FileFormat
import Relude.Container.One (one)
import Relude.Foldable (foldMap)
import Relude.Function (($))
import Relude.Monad (Maybe (Nothing))
import Relude.Monoid ((<>))

gen :: Exec.Exec ()
gen = do
  pnpmWorkspace <- Exec.readFile "pnpm-workspace.yaml" FileFormat.YAML
  rootTaskfileInput <- Exec.readFile "Taskfile.yml" FileFormat.YAML
  Exec.writeFile "Taskfile.out.yml" FileFormat.YAML $
    genRootTaskfile
      pnpmWorkspace
      rootTaskfileInput

genRootTaskfile :: FileFormat.PNPMWorkspace -> FileFormat.Taskfile -> FileFormat.Taskfile
genRootTaskfile pnpmWorkspace rootTaskfileInput =
  FileFormat.Taskfile
    { version = FileFormat.taskfileVersion,
      run = FileFormat.taskfileRun,
      includes =
        rootTaskfileInput.includes
          <> ( \package ->
                 one (package, FileFormat.TaskfileInclude {internal = Nothing, taskfile = package})
             )
            `foldMap` pnpmWorkspace.packages,
      tasks = rootTaskfileInput.tasks
    }
