import Dev.Gen (gen)
import Dev.Gen.ExecSpec qualified as ExecSpec
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.InterpretSpec qualified as InterpretSpec
import Dev.Gen.PackageSpec qualified as PackageSpec
import Relude.Bool (Bool (True))
import Relude.Function (($), (.))
import Relude.Monad (Maybe (Just, Nothing), liftIO)
import Test.Hspec (Spec, context, hspec, specify)
import UnliftIO (MonadUnliftIO)

main :: (MonadUnliftIO m) => m ()
main = liftIO . hspec $ do
  spec
  PackageSpec.spec

spec :: Spec
spec = context "Gen" $ do
  context "gen" $ do
    specify "OK" $
      InterpretSpec.interpret
        gen
        [ ExecSpec.readFile
            "pnpm-workspace.yaml"
            FileFormat.YAML
            ( FileFormat.PNPMWorkspace
                { packages = ["a", "b"]
                }
            ),
          ExecSpec.readFile
            "a/package.json"
            FileFormat.JSON
            ( FileFormat.PNPMPackageFile
                { dependencies = Nothing,
                  devDependencies = Nothing
                }
            ),
          ExecSpec.readFile
            "b/package.json"
            FileFormat.JSON
            ( FileFormat.PNPMPackageFile
                { dependencies =
                    Just
                      [ ("@intertwine/a", "*")
                      ],
                  devDependencies = Nothing
                }
            ),
          ExecSpec.readFile
            "Taskfile.in.yml"
            FileFormat.YAML
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
                  tasks =
                    [ ( "y",
                        FileFormat.TaskfileTask
                          { deps = Just ["y"]
                          }
                      )
                    ]
                }
            ),
          ExecSpec.writeFile
            "a/Taskfile.yml"
            FileFormat.YAML
            ( FileFormat.Taskfile
                { version = FileFormat.taskfileVersion,
                  run = FileFormat.taskfileRun,
                  includes = Nothing,
                  tasks =
                    [ ( "build",
                        FileFormat.TaskfileTask
                          { deps = Just []
                          }
                      )
                    ]
                }
            ),
          ExecSpec.writeFile
            "b/Taskfile.yml"
            FileFormat.YAML
            ( FileFormat.Taskfile
                { version = FileFormat.taskfileVersion,
                  run = FileFormat.taskfileRun,
                  includes = Nothing,
                  tasks =
                    [ ( "build",
                        FileFormat.TaskfileTask
                          { deps = Just [":a:build"]
                          }
                      )
                    ]
                }
            ),
          ExecSpec.writeFile
            "Taskfile.yml"
            FileFormat.YAML
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
                  tasks =
                    [ ( "pnpm:build",
                        FileFormat.TaskfileTask
                          { deps = Just ["a:build", "b:build"]
                          }
                      ),
                      ( "y",
                        FileFormat.TaskfileTask
                          { deps = Just ["y"]
                          }
                      )
                    ]
                }
            )
        ]
        ()
