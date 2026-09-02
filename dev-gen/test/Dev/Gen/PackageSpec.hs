module Dev.Gen.PackageSpec
  ( spec,
  )
where

import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.Package qualified as Package
import Relude.Function (($))
import Relude.Monad (Maybe (Nothing))
import Test.Hspec (Spec, context, shouldBe, specify)

spec :: Spec
spec = context "Gen.PackageSpec" $ do
  context "transformPNPM" $ do
    specify "null" $
      Package.transformPNPM [] `shouldBe` []
    specify "preserves workspace names" $
      shouldBe
        ( Package.transformPNPM
            [ ( "a",
                FileFormat.PNPMPackageFile
                  { name = "@proj/a",
                    dependencies = Nothing,
                    devDependencies = Nothing
                  }
              ),
              ( "b",
                FileFormat.PNPMPackageFile
                  { name = "@proj/b",
                    dependencies = Nothing,
                    devDependencies = Nothing
                  }
              )
            ]
        )
        [ Package.PNPM
            { name = "a"
            },
          Package.PNPM
            { name = "b"
            }
        ]
