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
        [ ExecSpec.readTOML
            "Cargo.toml"
            ( FileFormat.CargoWorkspace
                { workspace =
                    FileFormat.CargoWorkspaceWorkspace
                      { members =
                          [ "dev-c",
                            "svc-d"
                          ]
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
                  devDependencies =
                    Just
                      [ ("typescript", "*")
                      ]
                }
            ),
          ExecSpec.readJSON
            "b/package.json"
            ( FileFormat.PNPMPackageFile
                { name = "@proj/b",
                  dependencies =
                    Just
                      [ ("@proj/a", "*")
                      ],
                  devDependencies =
                    Just
                      [ ("typescript", "*")
                      ]
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
          ExecSpec.writeJSON
            ".eslintrc.json"
            ( FileFormat.ESLintConfig
                { extends = FileFormat.esLintConfigExtends,
                  parserOptions =
                    FileFormat.ESLintConfigParserOptions
                      { tsconfigRootDir =
                          FileFormat.esLintConfigParserOptionsTsconfigRootDir,
                        project = ["a/tsconfig.json", "b/tsconfig.json"]
                      }
                }
            ),
          ExecSpec.writeYAML
            "dev-c/Taskfile.yml"
            ( FileFormat.Taskfile
                { version = FileFormat.taskfileVersion,
                  run = FileFormat.taskfileRun,
                  includes = Nothing,
                  vars = Just [("NAME", "dev-c")],
                  tasks =
                    [ ( "test:debug",
                        FileFormat.TaskfileTask
                          { aliases = Just ["test", "t"],
                            deps = Nothing,
                            cmd =
                              Just
                                ( FileFormat.TaskfileCommand
                                    { task = ":cargo:execute-package:test:debug",
                                      vars = Just [("NAME", "{{.NAME}}")]
                                    }
                                ),
                            cmds = Nothing
                          }
                      ),
                      ( "test:debug:watch",
                        FileFormat.TaskfileTask
                          { aliases = Just ["test:watch", "tw"],
                            deps = Nothing,
                            cmd =
                              Just
                                ( FileFormat.TaskfileCommand
                                    { task = ":cargo:execute-package:test:debug:watch",
                                      vars = Just [("NAME", "{{.NAME}}")]
                                    }
                                ),
                            cmds = Nothing
                          }
                      ),
                      ( "test:release",
                        FileFormat.TaskfileTask
                          { aliases = Just ["tr"],
                            deps = Nothing,
                            cmd =
                              Just
                                ( FileFormat.TaskfileCommand
                                    { task = ":cargo:execute-package:test:release",
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
            "svc-d/Taskfile.yml"
            ( FileFormat.Taskfile
                { version = FileFormat.taskfileVersion,
                  run = FileFormat.taskfileRun,
                  includes = Nothing,
                  vars = Just [("NAME", "svc-d")],
                  tasks =
                    [ ( "run:debug",
                        FileFormat.TaskfileTask
                          { aliases = Just ["run", "r"],
                            deps = Nothing,
                            cmd =
                              Just
                                ( FileFormat.TaskfileCommand
                                    { task = ":cargo:execute-package:run:debug",
                                      vars = Just [("NAME", "{{.NAME}}")]
                                    }
                                ),
                            cmds = Nothing
                          }
                      ),
                      ( "run:debug:watch",
                        FileFormat.TaskfileTask
                          { aliases = Just ["run:watch", "rw"],
                            deps = Nothing,
                            cmd =
                              Just
                                ( FileFormat.TaskfileCommand
                                    { task = ":cargo:execute-package:run:debug:watch",
                                      vars = Just [("NAME", "{{.NAME}}")]
                                    }
                                ),
                            cmds = Nothing
                          }
                      ),
                      ( "run:release",
                        FileFormat.TaskfileTask
                          { aliases = Just ["rr"],
                            deps = Nothing,
                            cmd =
                              Just
                                ( FileFormat.TaskfileCommand
                                    { task = ":cargo:execute-package:run:release",
                                      vars = Just [("NAME", "{{.NAME}}")]
                                    }
                                ),
                            cmds = Nothing
                          }
                      ),
                      ( "test:debug",
                        FileFormat.TaskfileTask
                          { aliases = Just ["test", "t"],
                            deps = Nothing,
                            cmd =
                              Just
                                ( FileFormat.TaskfileCommand
                                    { task = ":cargo:execute-package:test:debug",
                                      vars = Just [("NAME", "{{.NAME}}")]
                                    }
                                ),
                            cmds = Nothing
                          }
                      ),
                      ( "test:debug:watch",
                        FileFormat.TaskfileTask
                          { aliases = Just ["test:watch", "tw"],
                            deps = Nothing,
                            cmd =
                              Just
                                ( FileFormat.TaskfileCommand
                                    { task = ":cargo:execute-package:test:debug:watch",
                                      vars = Just [("NAME", "{{.NAME}}")]
                                    }
                                ),
                            cmds = Nothing
                          }
                      ),
                      ( "test:release",
                        FileFormat.TaskfileTask
                          { aliases = Just ["tr"],
                            deps = Nothing,
                            cmd =
                              Just
                                ( FileFormat.TaskfileCommand
                                    { task = ":cargo:execute-package:test:release",
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
                                    { task =
                                        ":tmpfs:link-package-build-dir",
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
                                    { task =
                                        ":tmpfs:link-package-build-dir",
                                      vars = Just [("NAME", "{{.NAME}}")]
                                    }
                                ),
                            cmds = Nothing
                          }
                      )
                    ]
                }
            ),
          ExecSpec.writeJSON
            "a/tsconfig.json"
            ( FileFormat.TypeScriptConfig
                { extends = FileFormat.typeScriptConfigExtends,
                  include = FileFormat.typeScriptConfigInclude,
                  exclude = FileFormat.typeScriptConfigExclude,
                  compilerOptions =
                    FileFormat.typeScriptConfigCompilerOptions,
                  references = []
                }
            ),
          ExecSpec.writeJSON
            "b/tsconfig.json"
            ( FileFormat.TypeScriptConfig
                { extends = FileFormat.typeScriptConfigExtends,
                  include = FileFormat.typeScriptConfigInclude,
                  exclude = FileFormat.typeScriptConfigExclude,
                  compilerOptions =
                    FileFormat.typeScriptConfigCompilerOptions,
                  references =
                    [ FileFormat.TypeScriptConfigReference {path = "../a"}
                    ]
                }
            ),
          ExecSpec.writeLines
            "Procfile"
            [ "y: task y",
              "dev-c__test: task dev-c:test:watch",
              "svc-d__test: task svc-d:test:watch",
              "svc-d__run: task svc-d:run:watch"
            ],
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
                        ( "dev-c",
                          FileFormat.TaskfileInclude
                            { internal = Nothing,
                              taskfile = "dev-c"
                            }
                        ),
                        ( "svc-d",
                          FileFormat.TaskfileInclude
                            { internal = Nothing,
                              taskfile = "svc-d"
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
                            deps =
                              Just
                                [ "dev-c:test:debug",
                                  "svc-d:test:debug"
                                ],
                            cmd = Nothing,
                            cmds = Nothing
                          }
                      ),
                      ( "cargo:test:release",
                        FileFormat.TaskfileTask
                          { aliases = Just ["cargo:tr"],
                            deps =
                              Just
                                [ "dev-c:test:release",
                                  "svc-d:test:release"
                                ],
                            cmd = Nothing,
                            cmds = Nothing
                          }
                      ),
                      ( "pnpm:link-build-dirs",
                        FileFormat.TaskfileTask
                          { aliases = Nothing,
                            deps =
                              Just
                                [ "a:link-build-dir",
                                  "b:link-build-dir"
                                ],
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
          ExecSpec.writeJSON
            "tsconfig.json"
            ( FileFormat.TypeScriptConfig
                { extends = FileFormat.typeScriptConfigExtends,
                  include = [],
                  exclude = [],
                  compilerOptions =
                    FileFormat.typeScriptConfigCompilerOptions,
                  references =
                    [ FileFormat.TypeScriptConfigReference {path = "./a"},
                      FileFormat.TypeScriptConfigReference {path = "./b"}
                    ]
                }
            ),
          ExecSpec.writeLines
            ".sqlfluffignore"
            ["build", "tmp"],
          ExecSpec.writeJSON
            ".watchmanconfig"
            ( FileFormat.WatchmanConfig
                { ignoreDirs =
                    ["build", "tmp"]
                }
            )
        ]
        ()
