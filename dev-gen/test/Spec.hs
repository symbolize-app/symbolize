import Control.Monad.IO.Unlift (MonadUnliftIO)
import Dev.Gen (gen)
import Dev.Gen.ExecSpec qualified as ExecSpec
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.InterpretSpec qualified as InterpretSpec
import Relude.Bool (Bool (True))
import Relude.Function (($), (.))
import Relude.Monad (Maybe (Just, Nothing), liftIO)
import Test.Hspec (Spec, context, hspec, specify)

main :: (MonadUnliftIO m) => m ()
main = liftIO . hspec $ do
  spec

spec :: Spec
spec = context "Gen" $ do
  context "gen" $ do
    specify "emits the Gleam and JavaScript build graph" $
      InterpretSpec.interpret
        gen
        [ ExecSpec.readLines
            ".buckconfig.in"
            [ "[cells]",
              "  root = ."
            ],
          ExecSpec.readTOML
            "workspace.bzl"
            ( FileFormat.Workspace
                { members = []
                }
            ),
          ExecSpec.readLines
            ".gitignore"
            ["build", "tmp"],
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
          ExecSpec.readVendorTargets
            [("foo-bar", "//foo-bar-1.0.0:foo-bar")],
          ExecSpec.writeLines
            ".buckconfig"
            [ "[cells]",
              "  root = .",
              "",
              "[project]",
              "  ignore = \\",
              "    .git, \\",
              "    build, \\",
              "    tmp"
            ],
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
                      [ ( "z",
                          FileFormat.TaskfileInclude
                            { internal = Just True,
                              taskfile = "z"
                            }
                        )
                      ],
                  vars = Just [("v1", "v2")],
                  tasks =
                    [ ( "rust:test:debug",
                        FileFormat.TaskfileTask
                          { aliases = Just ["rust:test", "rust:t"],
                            deps = Just [],
                            cmd = Nothing,
                            cmds = Nothing
                          }
                      ),
                      ( "rust:test:release",
                        FileFormat.TaskfileTask
                          { aliases = Just ["rust:tr"],
                            deps = Just [],
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
            ["build", "tmp", "vendor"],
          ExecSpec.writeLines
            "vendor/BUCK"
            [ "load(\"//:rules.bzl\", \"alias\")",
              "",
              "alias(",
              "    name = \"foo-bar\",",
              "    actual = \"//foo-bar-1.0.0:foo-bar\",",
              ")",
              "",
              "alias(",
              "    name = \"foo_bar\",",
              "    actual = \"//foo-bar-1.0.0:foo-bar\",",
              ")"
            ],
          ExecSpec.writeJSON
            ".watchmanconfig"
            ( FileFormat.WatchmanConfig
                { ignoreDirs = ["build", "tmp"]
                }
            )
        ]
        ()
