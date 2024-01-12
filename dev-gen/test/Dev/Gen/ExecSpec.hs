module Dev.Gen.ExecSpec
  ( Result (..),
    readJSON,
    readLines,
    readTOML,
    readYAML,
    writeJSON,
    writeLines,
    writeYAML,
  )
where

import Data.Aeson qualified as Aeson
import Data.Vector (Vector)
import Dev.Gen.Command qualified as Command
import Dev.Gen.FilePath (FilePath)
import Relude.Base (Eq, Show, Type, Typeable)
import Relude.String (Text)
import Toml.FromValue qualified as Toml

type Result :: Type
data Result where
  Result ::
    (Typeable a, Eq a, Show a) =>
    Command.Command a ->
    a ->
    Result

deriving stock instance Show Result

readJSON ::
  (Aeson.FromJSON a, Eq a, Show a, Typeable a) =>
  FilePath ->
  a ->
  Result
readJSON = _result1 (Command.ReadJSON Command.JSON)

writeJSON ::
  (Aeson.ToJSON a, Eq a, Show a, Typeable a) =>
  FilePath ->
  a ->
  Result
writeJSON = _result2' (Command.WriteJSON Command.JSON)

readYAML ::
  (Aeson.FromJSON a, Eq a, Show a, Typeable a) =>
  FilePath ->
  a ->
  Result
readYAML = _result1 (Command.ReadJSON Command.YAML)

writeYAML ::
  (Aeson.ToJSON a, Eq a, Show a, Typeable a) =>
  FilePath ->
  a ->
  Result
writeYAML = _result2' (Command.WriteJSON Command.YAML)

readLines :: FilePath -> Vector Text -> Result
readLines = _result1 Command.ReadLines

writeLines :: FilePath -> Vector Text -> Result
writeLines = _result2' Command.WriteLines

readTOML ::
  (Toml.FromValue a, Eq a, Show a, Typeable a) =>
  FilePath ->
  a ->
  Result
readTOML = _result1 Command.ReadTOML

_result1 ::
  (Typeable b, Eq b, Show b) =>
  (a -> Command.Command b) ->
  a ->
  b ->
  Result
_result1 command a = Result (command a)

_result2' :: (a -> b -> Command.Command ()) -> a -> b -> Result
_result2' command a b = Result (command a b) ()
