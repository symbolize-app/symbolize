module Dev.Gen.FileFormat
  ( CargoWorkspace (..),
    CargoWorkspaceWorkspace (..),
    PNPMPackageFile (..),
    PNPMWorkspace (..),
    Taskfile (..),
    TaskfileCommand (..),
    TaskfileInclude (..),
    TaskfileTask (..),
    TypeScriptConfig (..),
    TypeScriptConfigCompilerOptions (..),
    TypeScriptConfigReference (..),
    WatchmanConfig (..),
    taskfileRun,
    taskfileVersion,
    typeScriptConfigCompilerOptions,
    typeScriptConfigCompilerOptionsDeclarationDir,
    typeScriptConfigCompilerOptionsPaths,
    typeScriptConfigExclude,
    typeScriptConfigExtends,
    typeScriptConfigInclude,
  )
where

import Data.Aeson qualified as Aeson
import Data.Vector (Vector)
import Relude.Base (Eq, Generic, Show, Type)
import Relude.Bool (Bool (True))
import Relude.Container (Map, fromList)
import Relude.Function ((.))
import Relude.Functor ((<$>))
import Relude.Monad (Maybe)
import Relude.String (Text)
import Toml.FromValue qualified as Toml

type CargoWorkspace :: Type
newtype CargoWorkspace = CargoWorkspace
  { workspace :: CargoWorkspaceWorkspace
  }
  deriving stock (Show, Eq)

instance Toml.FromValue CargoWorkspace where
  fromValue = Toml.parseTableFromValue (CargoWorkspace <$> Toml.reqKey "workspace")

type CargoWorkspaceWorkspace :: Type
newtype CargoWorkspaceWorkspace = CargoWorkspaceWorkspace
  { members :: Vector Text
  }
  deriving stock (Show, Eq)

instance Toml.FromValue CargoWorkspaceWorkspace where
  fromValue = Toml.parseTableFromValue (CargoWorkspaceWorkspace . fromList <$> Toml.reqKey "members")

type PNPMPackageFile :: Type
data PNPMPackageFile = PNPMPackageFile
  { name :: Text,
    dependencies :: Maybe (Map Text Text),
    devDependencies :: Maybe (Map Text Text)
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.FromJSON PNPMPackageFile

type TypeScriptConfig :: Type
data TypeScriptConfig = TypeScriptConfig
  { extends :: Text,
    include :: Vector Text,
    exclude :: Vector Text,
    compilerOptions :: TypeScriptConfigCompilerOptions,
    references :: Vector TypeScriptConfigReference
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON TypeScriptConfig

type PNPMWorkspace :: Type
newtype PNPMWorkspace = PNPMWorkspace
  { packages :: Vector Text
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.FromJSON PNPMWorkspace

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
    vars :: Maybe (Map Text Text),
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
data TaskfileTask = TaskfileTask
  { aliases :: Maybe (Vector Text),
    deps :: Maybe (Vector Text),
    cmd :: Maybe TaskfileCommand,
    cmds :: Maybe (Vector Text)
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON TaskfileTask where
  toJSON :: TaskfileTask -> Aeson.Value
  toJSON = Aeson.genericToJSON taskfileOptions
  toEncoding :: TaskfileTask -> Aeson.Encoding
  toEncoding = Aeson.genericToEncoding taskfileOptions

instance Aeson.FromJSON TaskfileTask

type TaskfileCommand :: Type
data TaskfileCommand = TaskfileCommand
  { task :: Text,
    vars :: Maybe (Map Text Text)
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON TaskfileCommand where
  toJSON :: TaskfileCommand -> Aeson.Value
  toJSON = Aeson.genericToJSON taskfileOptions
  toEncoding :: TaskfileCommand -> Aeson.Encoding
  toEncoding = Aeson.genericToEncoding taskfileOptions

instance Aeson.FromJSON TaskfileCommand

typeScriptConfigExtends :: Text
typeScriptConfigExtends = "@intertwine/dev-tsconfig/tsconfig.json"

typeScriptConfigInclude :: Vector Text
typeScriptConfigInclude = ["./**/*.ts", "./**/*.js", "./**/*.cjs"]

typeScriptConfigExclude :: Vector Text
typeScriptConfigExclude = ["build/**", "node_modules/**"]

type TypeScriptConfigCompilerOptions :: Type
data TypeScriptConfigCompilerOptions = TypeScriptConfigCompilerOptions
  { declarationDir :: Text,
    paths :: Map Text (Vector Text),
    tsBuildInfoFile :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON TypeScriptConfigCompilerOptions

typeScriptConfigCompilerOptions :: TypeScriptConfigCompilerOptions
typeScriptConfigCompilerOptions =
  TypeScriptConfigCompilerOptions
    { declarationDir = typeScriptConfigCompilerOptionsDeclarationDir,
      paths = typeScriptConfigCompilerOptionsPaths,
      tsBuildInfoFile = typeScriptConfigCompilerOptionsTSBuildInfoFile
    }

typeScriptConfigCompilerOptionsDeclarationDir :: Text
typeScriptConfigCompilerOptionsDeclarationDir = "./build/tsc"

typeScriptConfigCompilerOptionsPaths :: Map Text (Vector Text)
typeScriptConfigCompilerOptionsPaths =
  [ ("@/*.ts", ["./*.ts"]),
    ("@/*.sql", ["./*.sql"])
  ]

typeScriptConfigCompilerOptionsTSBuildInfoFile :: Text
typeScriptConfigCompilerOptionsTSBuildInfoFile =
  "build/tsc/tsconfig.tsbuildinfo"

type TypeScriptConfigReference :: Type
newtype TypeScriptConfigReference = TypeScriptConfigReference
  { path :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON TypeScriptConfigReference

watchmanConfigOptions :: Aeson.Options
watchmanConfigOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_'
    }

type WatchmanConfig :: Type
newtype WatchmanConfig = WatchmanConfig
  { ignoreDirs :: Vector Text
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON WatchmanConfig where
  toJSON :: WatchmanConfig -> Aeson.Value
  toJSON = Aeson.genericToJSON watchmanConfigOptions
  toEncoding :: WatchmanConfig -> Aeson.Encoding
  toEncoding = Aeson.genericToEncoding watchmanConfigOptions
