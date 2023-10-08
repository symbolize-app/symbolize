module Dev.Gen
  ( gen,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.Vector (Vector)
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.FileFormat qualified as FileFormat
import Relude.Applicative (Applicative (pure))
import Relude.Base (Eq, Generic, Show, Type)
import Relude.Monad (Either (Left, Right), fail)
import Relude.Numeric (Integer)
import Relude.String (Text)

gen :: Exec.Exec PNPMWorkspace
gen = do
  pnpmWorkspaceDoc <- Exec.readFile "../pnpm-workspace.yaml" FileFormat.YAML
  case Aeson.Types.parseEither Aeson.parseJSON pnpmWorkspaceDoc of
    (Left error) -> fail error
    (Right pnpmWorkspace) -> pure pnpmWorkspace

type PNPMWorkspace :: Type
newtype PNPMWorkspace = PNPMWorkspace
  { packages :: Vector Text
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.FromJSON PNPMWorkspace

type WorkspaceTaskfile :: Type
newtype WorkspaceTaskfile = WorkspaceTaskfile
  { tasks :: Vector Task
  }
  deriving newtype (Show, Eq)

type Task :: Type
newtype Task = Task
  { dependencies :: Vector TaskReference
  }
  deriving newtype (Show, Eq)

type TaskReference :: Type
data TaskReference = Dependency
  { pop :: Integer,
    push :: Vector Text
  }
  deriving stock (Show, Eq)
