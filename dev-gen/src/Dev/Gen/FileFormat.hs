module Dev.Gen.FileFormat
  ( Storage (..),
    PNPMWorkspace (..),
    PNPMPackage (..),
    Taskfile (..),
    TaskfileTask (..),
    TaskfileInclude (..),
    taskfileRun,
    taskfileVersion,
  )
where

import Data.Aeson qualified as Aeson
import Data.Vector (Vector)
import Relude.Base (Eq, Generic, Show, Type)
import Relude.Bool (Bool (True))
import Relude.Container (Map)
import Relude.Monad (Maybe)
import Relude.String (Text)

type Storage :: Type
data Storage where
  JSON :: Storage
  YAML :: Storage

deriving stock instance Eq Storage

deriving stock instance Show Storage

type PNPMWorkspace :: Type
newtype PNPMWorkspace = PNPMWorkspace
  { packages :: Vector Text
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.FromJSON PNPMWorkspace

type PNPMPackage :: Type
data PNPMPackage = PNPMPackage
  { dependencies :: Maybe (Map Text Text),
    devDependencies :: Maybe (Map Text Text)
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.FromJSON PNPMPackage

taskfileOptions :: Aeson.Options
taskfileOptions =
  Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    }

type Taskfile :: Type
data Taskfile = Taskfile
  { version :: Text,
    run :: Text,
    includes :: Maybe (Map Text TaskfileInclude),
    tasks :: Map Text TaskfileTask
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON Taskfile where
  toJSON :: Taskfile -> Aeson.Value
  toJSON = Aeson.genericToJSON taskfileOptions
  toEncoding :: Taskfile -> Aeson.Encoding
  toEncoding = Aeson.genericToEncoding taskfileOptions

instance Aeson.FromJSON Taskfile

taskfileVersion :: Text
taskfileVersion = "3"

taskfileRun :: Text
taskfileRun = "when_changed"

type TaskfileInclude :: Type
data TaskfileInclude = TaskfileInclude
  { internal :: Maybe Bool,
    taskfile :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON TaskfileInclude where
  toJSON :: TaskfileInclude -> Aeson.Value
  toJSON = Aeson.genericToJSON taskfileOptions
  toEncoding :: TaskfileInclude -> Aeson.Encoding
  toEncoding = Aeson.genericToEncoding taskfileOptions

instance Aeson.FromJSON TaskfileInclude

type TaskfileTask :: Type
newtype TaskfileTask = TaskfileTask
  { deps :: Vector Text
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON TaskfileTask where
  toJSON :: TaskfileTask -> Aeson.Value
  toJSON = Aeson.genericToJSON taskfileOptions
  toEncoding :: TaskfileTask -> Aeson.Encoding
  toEncoding = Aeson.genericToEncoding taskfileOptions

instance Aeson.FromJSON TaskfileTask
