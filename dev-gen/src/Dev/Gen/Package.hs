module Dev.Gen.Package
  ( PNPM (..),
    TypeScript (..),
    transformPNPM,
    foldTypeScriptPackageNames,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Dev.Gen.FileFormat qualified as FileFormat
import Relude.Applicative (empty, pure)
import Relude.Base (Eq, Show, Type)
import Relude.Container (Map, fromList)
import Relude.Function (($), (.))
import Relude.Functor ((<$>))
import Relude.Monad (Maybe, isJust, mapMaybe)
import Relude.Monoid (maybeToMonoid, (<>))
import Relude.String (Text)

type PNPM :: Type
data PNPM = PNPM
  { name :: Text,
    typeScript :: Maybe TypeScript
  }
  deriving stock (Show, Eq)

type TypeScript :: Type
newtype TypeScript = TypeScript
  { dependencies :: Vector Text
  }
  deriving stock (Show, Eq)

transformPNPM :: Map Text FileFormat.PNPMPackageFile -> Vector PNPM
transformPNPM pnpmPackageFiles =
  let packagesMap :: Map Text PNPM
      packagesMap = transformOne `Map.mapWithKey` pnpmPackageFiles
      transformOne :: Text -> FileFormat.PNPMPackageFile -> PNPM
      transformOne name pnpmPackageFile = do
        PNPM
          { name = name,
            typeScript = transformTypeScript pnpmPackageFile
          }
      transformTypeScript :: FileFormat.PNPMPackageFile -> Maybe TypeScript
      transformTypeScript pnpmPackageFile = do
        devDependencies <- pnpmPackageFile.devDependencies
        if Map.member "typescript" devDependencies
          then do
            let dependencies =
                  fromList . mapMaybe findDependency $
                    Map.keys (maybeToMonoid pnpmPackageFile.dependencies)
                      <> Map.keys devDependencies
            pure
              TypeScript
                { dependencies = dependencies
                }
          else empty
      findDependency :: Text -> Maybe Text
      findDependency name = do
        name' <- Text.stripPrefix "@intertwine/" name
        package <- Map.lookup name' packagesMap
        if isJust package.typeScript then pure name' else empty
   in fromList $ Map.elems packagesMap

foldTypeScriptPackageNames :: Vector PNPM -> Vector Text
foldTypeScriptPackageNames =
  ((.name) <$>)
    . Vector.filter (\pnpmPackage -> isJust pnpmPackage.typeScript)
