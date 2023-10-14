module Dev.Gen
  ( gen,
    PNPMWorkspace (..),
  )
where

import Data.Aeson qualified as Aeson
import Data.Vector (Vector, length)
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.FileFormat qualified as FileFormat
import Relude (Applicative (pure))
import Relude.Base (Eq, Generic, Show, Type)
import Relude.Numeric (Int, Integer)
import Relude.String (Text)

gen :: Exec.Exec (Int, PNPMWorkspace)
gen = do
  pnpmWorkspace <- Exec.readFile "pnpm-workspace.yaml" FileFormat.YAML
  pure (length pnpmWorkspace.packages, pnpmWorkspace)

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
