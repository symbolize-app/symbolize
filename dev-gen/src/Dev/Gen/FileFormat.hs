module Dev.Gen.FileFormat
  ( FileFormat (..),
  )
where

import Relude.Base (Eq, Show, Type)

type FileFormat :: Type
data FileFormat where
  JSON :: FileFormat
  YAML :: FileFormat

deriving stock instance Eq FileFormat

deriving stock instance Show FileFormat
