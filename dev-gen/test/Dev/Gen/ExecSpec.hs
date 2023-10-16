module Dev.Gen.ExecSpec
  ( Result (..),
    readFile,
    writeFile,
  )
where

import Data.Aeson qualified as Aeson
import Dev.Gen.Command qualified as Command
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.FilePath (FilePath)
import Relude.Base (Eq, Show, Type, Typeable)

type Result :: Type
data Result where
  Result :: (Typeable a, Eq a, Show a) => Command.Command a -> a -> Result

deriving stock instance Show Result

readFile :: (Aeson.FromJSON a, Eq a, Show a, Typeable a) => FilePath -> FileFormat.Storage -> a -> Result
readFile = _result2 Command.ReadFile

writeFile :: (Aeson.ToJSON b, Eq b, Show b, Typeable b) => FilePath -> FileFormat.Storage -> b -> Result
writeFile = _result3' Command.WriteFile

_result2 :: (Typeable a, Eq a, Show a) => (t1 -> t2 -> Command.Command a) -> t1 -> t2 -> a -> Result
_result2 command b c = Result (command b c)

_result3' :: (t1 -> t2 -> t3 -> Command.Command ()) -> t1 -> t2 -> t3 -> Result
_result3' command b c d = Result (command b c d) ()
