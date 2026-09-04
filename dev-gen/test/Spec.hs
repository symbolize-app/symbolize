import Control.Monad.IO.Unlift (MonadUnliftIO)
import Dev.Gen (gen)
import Dev.Gen.ExecSpec qualified as ExecSpec
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.InterpretSpec qualified as InterpretSpec
import Dev.Gen.PackageSpec qualified as PackageSpec
import Relude.Bool (Bool (True))
import Relude.Function (($), (.))
import Relude.Monad (Maybe (Just, Nothing), liftIO)
import Test.Hspec (Spec, context, hspec, specify)

main :: (MonadUnliftIO m) => m ()
main = liftIO . hspec $ do
  spec
  PackageSpec.spec

spec :: Spec
spec = context "Gen" $ do
  context "gen" $ do
    specify "emits the Gleam and JavaScript build graph" $
      InterpretSpec.interpret
        gen
        [ ExecSpec.readTOML
            "Cargo.toml"
            ( FileFormat.CargoWorkspace
                { workspace =
                    FileFormat.CargoWorkspaceWorkspace
                      { members = []
                      }
                }
            ),
          ExecSpec.readLines
            ".gitignore"
            ["build", "tmp"],
          ExecSpec.readYAML
            "pnpm-workspace.yaml"
            ( FileFormat.PNPMWorkspace
                { packages = ["a", "b"]
                }
            ),
          ExecSpec.readJSON
            "a/package.json"
            ( FileFormat.PNPMPackageFile
                { name = "@proj/a",
                  dependencies = Nothing,
                  devDependencies = Nothing
                }
            ),
          ExecSpec.readJSON
            "b/package.json"
            ( FileFormat.PNPMPackageFile
                { name = "@proj/b",
                  dependencies = Just [("@proj/a", "*")],
                  devDependencies = Nothing
                }
            ),
          ExecSpec.readLines
            "Procfile.in"
            ["y: task y"],
          ExecSpec.readYAML
            "Taskfile.in.yml"
            ( FileFormat.Taskfile
                { version = FileFormat.taskfileVersion,
                  run = FileFormat.taskfileRun,
                  includes =
                    Just
                      [ ( "z",
                          FileFormat.TaskfileInclude
                            { internal = Just True,
                              taskfile = "z"
                            }
                        )
                      ],
                  vars = Just [("v1", "v2")],
                  tasks =
                    [ ( "y",
                        FileFormat.TaskfileTask
                          { aliases = Nothing,
                            deps = Just ["y"],
                            cmd = Nothing,
                            cmds = Nothing
                          }
                      )
                    ]
                }
            ),
          ExecSpec.writeLines
            "Procfile"
            ["y: task y"],
          ExecSpec.writeYAML
            "Taskfile.yml"
            ( FileFormat.Taskfile
                { version = FileFormat.taskfileVersion,
                  run = FileFormat.taskfileRun,
                  includes =
                    Just
                      [ ( "a",
                          FileFormat.TaskfileInclude
                            { internal = Nothing,
                              taskfile = "a"
                            }
                        ),
                        ( "b",
                          FileFormat.TaskfileInclude
                            { internal = Nothing,
                              taskfile = "b"
                            }
                        ),
                        ( "z",
                          FileFormat.TaskfileInclude
                            { internal = Just True,
                              taskfile = "z"
                            }
                        )
                      ],
                  vars = Just [("v1", "v2")],
                  tasks =
                    [ ( "cargo:test:debug",
                        FileFormat.TaskfileTask
                          { aliases = Just ["cargo:test", "cargo:t"],
                            deps = Just [],
                            cmd = Nothing,
                            cmds = Nothing
                          }
                      ),
                      ( "cargo:test:release",
                        FileFormat.TaskfileTask
                          { aliases = Just ["cargo:tr"],
                            deps = Just [],
                            cmd = Nothing,
                            cmds = Nothing
                          }
                      ),
                      ( "pnpm:link-build-dirs",
                        FileFormat.TaskfileTask
                          { aliases = Nothing,
                            deps = Just ["a:link-build-dir", "b:link-build-dir"],
                            cmd = Nothing,
                            cmds = Nothing
                          }
                      ),
                      ( "y",
                        FileFormat.TaskfileTask
                          { aliases = Nothing,
                            deps = Just ["y"],
                            cmd = Nothing,
                            cmds = Nothing
                          }
                      )
                    ]
                }
            ),
          ExecSpec.writeLines
            ".sqlfluffignore"
            ["build", "tmp"],
          ExecSpec.writeJSON
            ".watchmanconfig"
            ( FileFormat.WatchmanConfig
                { ignoreDirs = ["build", "tmp"]
                }
            )
        ]
        ()
