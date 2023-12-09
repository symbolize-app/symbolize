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
    specify "OK" $
      InterpretSpec.interpret
        gen
        [ ExecSpec.readLines
            ".gitignore"
            ["a", "b"],
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
          ExecSpec.readYAML
            "pnpm-workspace.yaml"
            ( FileFormat.PNPMWorkspace
                { packages = ["a", "b"]
                }
            ),
          ExecSpec.readJSON
            "a/package.json"
            ( FileFormat.PNPMPackageFile
                { dependencies = Nothing,
                  devDependencies =
                    Just
                      [ ("typescript", "*")
                      ]
                }
            ),
          ExecSpec.readJSON
            "b/package.json"
            ( FileFormat.PNPMPackageFile
                { dependencies =
                    Just
                      [ ("@intertwine/a", "*")
                      ],
                  devDependencies =
                    Just
                      [ ("typescript", "*")
                      ]
                }
            ),
          ExecSpec.writeLines
            ".sqlfluffignore"
            ["a", "b"],
          ExecSpec.writeJSON
            "a/tsconfig.json"
            ( FileFormat.TypeScriptConfig
                { extends = FileFormat.typeScriptConfigExtends,
                  include = FileFormat.typeScriptConfigInclude,
                  exclude = FileFormat.typeScriptConfigExclude,
                  compilerOptions = FileFormat.typeScriptConfigCompilerOptions,
                  references = []
                }
            ),
          ExecSpec.writeJSON
            "b/tsconfig.json"
            ( FileFormat.TypeScriptConfig
                { extends = FileFormat.typeScriptConfigExtends,
                  include = FileFormat.typeScriptConfigInclude,
                  exclude = FileFormat.typeScriptConfigExclude,
                  compilerOptions = FileFormat.typeScriptConfigCompilerOptions,
                  references =
                    [ FileFormat.TypeScriptConfigReference {path = "../a"}
                    ]
                }
            ),
          ExecSpec.writeJSON
            "tsconfig.json"
            ( FileFormat.TypeScriptConfig
                { extends = FileFormat.typeScriptConfigExtends,
                  include = [],
                  exclude = [],
                  compilerOptions = FileFormat.typeScriptConfigCompilerOptions,
                  references =
                    [ FileFormat.TypeScriptConfigReference {path = "./a"},
                      FileFormat.TypeScriptConfigReference {path = "./b"}
                    ]
                }
            ),
          ExecSpec.writeJSON
            ".eslintrc.json"
            ( FileFormat.ESLintConfig
                { extends = FileFormat.esLintConfigExtends,
                  parserOptions =
                    FileFormat.ESLintConfigParserOptions
                      { tsconfigRootDir = FileFormat.esLintConfigParserOptionsTsconfigRootDir,
                        project = ["a/tsconfig.json", "b/tsconfig.json"]
                      }
                }
            ),
          ExecSpec.writeYAML
            "a/Taskfile.yml"
            ( FileFormat.Taskfile
                { version = FileFormat.taskfileVersion,
                  run = FileFormat.taskfileRun,
                  includes = Nothing,
                  vars =
                    Just
                      [("NAME", "a")],
                  tasks =
                    [ ( "link-build-dir",
                        FileFormat.TaskfileTask
                          { aliases = Nothing,
                            deps = Nothing,
                            cmd =
                              Just
                                ( FileFormat.TaskfileCommand
                                    { task = ":tmpfs:link-package-build-dir",
                                      vars = Just [("NAME", "{{.NAME}}")]
                                    }
                                ),
                            cmds = Nothing
                          }
                      )
                    ]
                }
            ),
          ExecSpec.writeYAML
            "b/Taskfile.yml"
            ( FileFormat.Taskfile
                { version = FileFormat.taskfileVersion,
                  run = FileFormat.taskfileRun,
                  includes = Nothing,
                  vars =
                    Just
                      [("NAME", "b")],
                  tasks =
                    [ ( "link-build-dir",
                        FileFormat.TaskfileTask
                          { aliases = Nothing,
                            deps = Nothing,
                            cmd =
                              Just
                                ( FileFormat.TaskfileCommand
                                    { task = ":tmpfs:link-package-build-dir",
                                      vars = Just [("NAME", "{{.NAME}}")]
                                    }
                                ),
                            cmds = Nothing
                          }
                      )
                    ]
                }
            ),
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
                    [ ( "y",
                        FileFormat.TaskfileTask
                          { aliases = Nothing,
                            deps = Just ["y"],
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
                      )
                    ]
                }
            )
        ]
        ()
