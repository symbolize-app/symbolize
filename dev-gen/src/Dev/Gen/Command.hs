module Dev.Gen.Command
  ( Command (..),
  )
where

import Data.Aeson qualified as Aeson
import Data.Typeable (cast)
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.FilePath (FilePath)
import Relude (Bool (False))
import Relude.Base (Eq ((==)), Show, Type, Typeable)
import Relude.Monad (maybe)

type Command :: Type -> Type
data Command a where
  ReadFile :: (Aeson.FromJSON a, Eq a, Show a, Typeable a) => FilePath -> FileFormat.Storage -> Command a
  WriteFile :: (Aeson.ToJSON b, Eq b, Show b, Typeable b) => FilePath -> FileFormat.Storage -> b -> Command ()

instance Eq (Command a) where
  (==) :: Command a -> Command a -> Bool
  (ReadFile filePathX fileFormatX) == (ReadFile filePathY fileFormatY) =
    (filePathX, fileFormatX) == (filePathY, fileFormatY)
  (WriteFile filePathX fileFormatX valueX) == (WriteFile filePathY fileFormatY valueY) =
    maybe
      False
      (\valueY' -> (filePathX, fileFormatX, valueX) == (filePathY, fileFormatY, valueY'))
      (cast valueY)
  _ == _ = False

deriving stock instance Show (Command a)
