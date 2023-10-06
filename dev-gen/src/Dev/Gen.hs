module Dev.Gen
  ( gen,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.FileFormat qualified as FileFormat
import Relude.Applicative (Applicative (pure))
import Relude.Base (Show, Type)
import Relude.Function (($))
import Relude.Monad (Either (Left, Right), fail)
import Relude.String (Text)

gen :: Exec.Exec PNPMWorkspace
gen = do
  pnpmWorkspaceDoc <- Exec.readFile "../pnpm-workspace.yaml" FileFormat.YAML
  case Aeson.Types.parseEither parsePNPMWorkspace pnpmWorkspaceDoc of
    (Left error) -> fail error
    (Right pnpmWorkspace) -> pure pnpmWorkspace

type PNPMWorkspace :: Type
data PNPMWorkspace where
  PNPMWorkspace :: [Text] -> PNPMWorkspace

deriving stock instance Show PNPMWorkspace

parsePNPMWorkspace :: Aeson.Types.Value -> Aeson.Types.Parser PNPMWorkspace
parsePNPMWorkspace doc = do
  workspace <- Aeson.parseJSON doc
  packages :: [Text] <- Aeson.Types.parseField workspace "packages"
  pure $ PNPMWorkspace packages
