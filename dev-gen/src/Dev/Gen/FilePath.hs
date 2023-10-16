module Dev.Gen.FilePath
  ( FilePath (..),
  )
where

import Relude (ToString)
import Relude.Base (Eq, Show, Type)
import Relude.String (IsString, Text, ToText)

type FilePath :: Type
newtype FilePath = FilePath Text
  deriving newtype (Eq, Show, ToString, ToText, IsString)
