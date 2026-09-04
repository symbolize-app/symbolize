module Dev.Gen.Package
  ( PNPM (..),
    isCargoService,
    transformPNPM,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Vector (Vector)
import Dev.Gen.FileFormat qualified as FileFormat
import Relude.Base (Eq, Show, Type)
import Relude.Bool (Bool)
import Relude.Container (fromList, uncurry)
import Relude.Foldable (toList)
import Relude.Function (($), (.))
import Relude.Functor ((<$>))
import Relude.String (Text)

type PNPM :: Type
newtype PNPM = PNPM
  { name :: Text
  }
  deriving stock (Show, Eq)

isCargoService :: Text -> Bool
isCargoService = Text.isPrefixOf "svc-"

transformPNPM :: Vector (Text, FileFormat.PNPMPackageFile) -> Vector PNPM
transformPNPM pnpmPackageFiles =
  fromList
    . Map.elems
    . Map.fromList
    $ uncurry transformOne <$> toList pnpmPackageFiles
  where
    transformOne :: Text -> FileFormat.PNPMPackageFile -> (Text, PNPM)
    transformOne name pnpmPackageFile =
      ( pnpmPackageFile.name,
        PNPM
          { name = name
          }
      )
