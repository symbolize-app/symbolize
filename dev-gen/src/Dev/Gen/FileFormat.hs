module Dev.Gen.FileFormat
  ( ESLintConfig (..),
    ESLintConfigParserOptions (..),
    PNPMPackageFile (..),
    PNPMWorkspace (..),
    Storage (..),
    Taskfile (..),
    TaskfileInclude (..),
    TaskfileTask (..),
    TypeScriptConfig (..),
    TypeScriptConfigCompilerOptions (..),
    TypeScriptConfigReference (..),
    esLintConfigExtends,
    esLintConfigParserOptionsTsconfigRootDir,
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

type PNPMPackageFile :: Type
data PNPMPackageFile = PNPMPackageFile
  { dependencies :: Maybe (Map Text Text),
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

typeScriptConfigExtends :: Text
typeScriptConfigExtends = "@intertwine/dev-tsconfig/tsconfig.json"

typeScriptConfigInclude :: Vector Text
typeScriptConfigInclude = ["./**/*.ts", "./**/*.js", "./**/*.cjs"]

typeScriptConfigExclude :: Vector Text
typeScriptConfigExclude = ["build/**"]

type TypeScriptConfigCompilerOptions :: Type
data TypeScriptConfigCompilerOptions = TypeScriptConfigCompilerOptions
  { declarationDir :: Text,
    paths :: Map Text (Vector Text)
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON TypeScriptConfigCompilerOptions

typeScriptConfigCompilerOptions :: TypeScriptConfigCompilerOptions
typeScriptConfigCompilerOptions =
  TypeScriptConfigCompilerOptions
    { declarationDir = typeScriptConfigCompilerOptionsDeclarationDir,
      paths = typeScriptConfigCompilerOptionsPaths
    }

typeScriptConfigCompilerOptionsDeclarationDir :: Text
typeScriptConfigCompilerOptionsDeclarationDir = "./build/.d.ts"

typeScriptConfigCompilerOptionsPaths :: Map Text (Vector Text)
typeScriptConfigCompilerOptionsPaths =
  [ ("@/*.ts", ["./*.ts"])
  ]

type TypeScriptConfigReference :: Type
newtype TypeScriptConfigReference = TypeScriptConfigReference
  { path :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON TypeScriptConfigReference

type ESLintConfig :: Type
data ESLintConfig = ESLintConfig
  { extends :: Vector Text,
    parserOptions :: ESLintConfigParserOptions
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON ESLintConfig

esLintConfigExtends :: Vector Text
esLintConfigExtends = ["@intertwine"]

type ESLintConfigParserOptions :: Type
data ESLintConfigParserOptions = ESLintConfigParserOptions
  { tsconfigRootDir :: Text,
    project :: Vector Text
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON ESLintConfigParserOptions

esLintConfigParserOptionsTsconfigRootDir :: Text
esLintConfigParserOptionsTsconfigRootDir = "."

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
    cmd :: Maybe TaskfileCommand
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
    vars :: Map Text Text
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON TaskfileCommand where
  toJSON :: TaskfileCommand -> Aeson.Value
  toJSON = Aeson.genericToJSON taskfileOptions
  toEncoding :: TaskfileCommand -> Aeson.Encoding
  toEncoding = Aeson.genericToEncoding taskfileOptions

instance Aeson.FromJSON TaskfileCommand
