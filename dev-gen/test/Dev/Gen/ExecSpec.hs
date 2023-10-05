module Dev.Gen.ExecSpec
  ( Result (..),
    readFile,
    writeFile,
  )
where

import Data.Aeson qualified as Aeson
import Dev.Gen.Command qualified as Command
import Dev.Gen.FileFormat qualified as FileFormat
import Relude.Base (FilePath, Show, Type)

type Result :: Type
data Result where
  Result :: (Show a) => Command.Command a -> a -> Result

deriving stock instance Show Result

readFile :: FilePath -> FileFormat.FileFormat -> Aeson.Value -> Result
readFile = _result2 Command.ReadFile

writeFile :: FilePath -> FileFormat.FileFormat -> Aeson.Value -> Result
writeFile = _result3' Command.WriteFile

_result2 :: (Show a) => (t1 -> t2 -> Command.Command a) -> t1 -> t2 -> a -> Result
_result2 command b c = Result (command b c)

_result3' :: (t1 -> t2 -> t3 -> Command.Command ()) -> t1 -> t2 -> t3 -> Result
_result3' command b c d = Result (command b c d) ()
