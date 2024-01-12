module Dev.Gen.Command
  ( Command (..),
    JSONStorage (..),
  )
where

import Data.Aeson qualified as Aeson
import Data.Typeable (cast)
import Data.Vector (Vector)
import Dev.Gen.FilePath (FilePath)
import Relude.Base (Eq ((==)), Show, Type, Typeable)
import Relude.Bool (Bool (False))
import Relude.Monad (maybe)
import Relude.String (Text)
import Toml.FromValue qualified as Toml

type Command :: Type -> Type
data Command a where
  ReadJSON ::
    (Aeson.FromJSON a, Eq a, Show a, Typeable a) =>
    JSONStorage ->
    FilePath ->
    Command a
  WriteJSON ::
    (Aeson.ToJSON b, Eq b, Show b, Typeable b) =>
    JSONStorage ->
    FilePath ->
    b ->
    Command ()
  ReadTOML ::
    (Toml.FromValue a, Eq a, Show a, Typeable a) =>
    FilePath ->
    Command a
  ReadLines :: FilePath -> Command (Vector Text)
  WriteLines :: FilePath -> Vector Text -> Command ()

instance Eq (Command a) where
  (==) :: Command a -> Command a -> Bool
  (ReadJSON fileFormatX filePathX)
    == (ReadJSON fileFormatY filePathY) =
      (fileFormatX, filePathX) == (fileFormatY, filePathY)
  (WriteJSON fileFormatX filePathX valueX)
    == (WriteJSON fileFormatY filePathY valueY) =
      maybe
        False
        ( \valueY' ->
            (fileFormatX, filePathX, valueX)
              == (fileFormatY, filePathY, valueY')
        )
        (cast valueY)
  (ReadLines filePathX) == (ReadLines filePathY) =
    filePathX == filePathY
  (WriteLines filePathX valueX) == (WriteLines filePathY valueY) =
    (filePathX, valueX) == (filePathY, valueY)
  (ReadTOML filePathX) == (ReadTOML filePathY) =
    filePathX == filePathY
  _ == _ = False

deriving stock instance Show (Command a)

type JSONStorage :: Type
data JSONStorage where
  JSON :: JSONStorage
  YAML :: JSONStorage

deriving stock instance Eq JSONStorage

deriving stock instance Show JSONStorage
