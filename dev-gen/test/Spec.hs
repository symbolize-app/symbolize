import Dev.Gen (gen)
import Dev.Gen.ExecSpec qualified as ExecSpec
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.InterpretSpec qualified as InterpretSpec
import Relude.Bool (Bool (True))
import Relude.Container (fromList)
import Relude.Function (($), (.))
import Relude.Monad (Maybe (Just, Nothing), MonadIO, liftIO)
import Test.Hspec (Spec, context, hspec, specify)

main :: (MonadIO m) => m ()
main = liftIO . hspec $ do
  spec

spec :: Spec
spec = do
  context "gen" $ do
    specify "OK" $
      InterpretSpec.interpret
        gen
        [ ExecSpec.readFile
            "pnpm-workspace.yaml"
            FileFormat.YAML
            ( FileFormat.PNPMWorkspace
                { packages = fromList ["a"]
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
                      ( fromList
                          [ ( "b",
                              FileFormat.TaskfileInclude
                                { internal = Just True,
                                  taskfile = "b"
                                }
                            )
                          ]
                      ),
                  tasks =
                    fromList
                      [ ( "c",
                          FileFormat.TaskfileTask
                            { deps = fromList ["c"]
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
                  tasks = fromList []
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
                      ( fromList
                          [ ( "a",
                              FileFormat.TaskfileInclude
                                { internal = Nothing,
                                  taskfile = "a"
                                }
                            ),
                            ( "b",
                              FileFormat.TaskfileInclude
                                { internal = Just True,
                                  taskfile = "b"
                                }
                            )
                          ]
                      ),
                  tasks =
                    fromList
                      [ ( "c",
                          FileFormat.TaskfileTask
                            { deps = fromList ["c"]
                            }
                        )
                      ]
                }
            )
        ]
        ()
