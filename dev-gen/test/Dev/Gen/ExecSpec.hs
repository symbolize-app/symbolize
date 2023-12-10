module Dev.Gen.ExecSpec
  ( Result (..),
    readJSON,
    readLines,
    readYAML,
    writeJSON,
    writeLines,
    writeYAML,
  )
where

import Data.Aeson qualified as Aeson
import Data.Vector (Vector)
import Dev.Gen.Command qualified as Command
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.FilePath (FilePath)
import Relude.Base (Eq, Show, Type, Typeable)
import Relude.String (Text)

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
readJSON = _result1 (Command.ReadJSON FileFormat.JSON)

writeJSON ::
  (Aeson.ToJSON b, Eq b, Show b, Typeable b) =>
  FilePath ->
  b ->
  Result
writeJSON = _result2' (Command.WriteJSON FileFormat.JSON)

readYAML ::
  (Aeson.FromJSON a, Eq a, Show a, Typeable a) =>
  FilePath ->
  a ->
  Result
readYAML = _result1 (Command.ReadJSON FileFormat.YAML)

writeYAML ::
  (Aeson.ToJSON b, Eq b, Show b, Typeable b) =>
  FilePath ->
  b ->
  Result
writeYAML = _result2' (Command.WriteJSON FileFormat.YAML)

readLines :: FilePath -> Vector Text -> Result
readLines = _result1 Command.ReadLines

writeLines :: FilePath -> Vector Text -> Result
writeLines = _result2' Command.WriteLines

_result1 ::
  (Typeable a, Eq a, Show a) =>
  (t1 -> Command.Command a) ->
  t1 ->
  a ->
  Result
_result1 command b = Result (command b)

_result2' :: (t1 -> t2 -> Command.Command ()) -> t1 -> t2 -> Result
_result2' command b c = Result (command b c) ()
