module Dev.Gen.PackageSpec
  ( spec,
  )
where

import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.Package qualified as Package
import Relude.Function (($))
import Relude.Monad (Maybe (Just, Nothing))
import Test.Hspec (Spec, context, shouldBe, specify)

spec :: Spec
spec = context "Gen.PackageSpec" $ do
  context "transformTypeScript" $ do
    specify "null" $
      Package.transformPNPM [] `shouldBe` []
    specify "simple dependency OK" $
      shouldBe
        ( Package.transformPNPM
            [ ( "a",
                FileFormat.PNPMPackageFile
                  { dependencies = Nothing,
                    devDependencies =
                      Just
                        [ ("typescript", "*")
                        ]
                  }
              ),
              ( "b",
                FileFormat.PNPMPackageFile
                  { dependencies =
                      Just
                        [ ("@intertwine/a", "*")
                        ],
                    devDependencies =
                      Just
                        [ ("typescript", "*")
                        ]
                  }
              )
            ]
        )
        [ Package.PNPM
            { name = "a",
              typeScript =
                Just
                  ( Package.TypeScript
                      { dependencies = []
                      }
                  )
            },
          Package.PNPM
            { name = "b",
              typeScript =
                Just
                  ( Package.TypeScript
                      { dependencies = ["a"]
                      }
                  )
            }
        ]
    specify "ignored dependency OK" $
      shouldBe
        ( Package.transformPNPM
            [ ( "a",
                FileFormat.PNPMPackageFile
                  { dependencies = Nothing,
                    devDependencies = Nothing
                  }
              ),
              ( "b",
                FileFormat.PNPMPackageFile
                  { dependencies =
                      Just
                        [ ("@intertwine/a", "*")
                        ],
                    devDependencies =
                      Just
                        [ ("typescript", "*")
                        ]
                  }
              )
            ]
        )
        [ Package.PNPM
            { name = "a",
              typeScript =
                Nothing
            },
          Package.PNPM
            { name = "b",
              typeScript =
                Just
                  ( Package.TypeScript
                      { dependencies = []
                      }
                  )
            }
        ]
