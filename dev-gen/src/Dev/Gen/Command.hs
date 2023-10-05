module Dev.Gen.Command
  ( Command (..),
  )
where

import Data.Aeson qualified as Aeson
import Dev.Gen.FileFormat qualified as FileFormat
import Relude.Base (Eq, FilePath, Show, Type)

type Command :: Type -> Type
data Command a where
  ReadFile :: FilePath -> FileFormat.FileFormat -> Command Aeson.Value
  WriteFile :: FilePath -> FileFormat.FileFormat -> Aeson.Value -> Command ()

deriving stock instance Eq (Command a)

deriving stock instance Show (Command a)
