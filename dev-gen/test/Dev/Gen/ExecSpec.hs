module Dev.Gen.ExecSpec
  ( Result (..),
    readFile,
    writeFile,
  )
where

import Dev.Gen.Command qualified as Command
import Named (NamedF (Arg), type (:!))
import Relude.Base (FilePath, Show, Type)
import Relude.String (Text)

type Result :: Type
data Result where
  Result :: (Show a) => Command.Command a -> a -> Result

deriving stock instance Show Result

readFile :: "path" :! FilePath -> "result" :! Text -> Result
readFile path (Arg result) = Result (Command.ReadFile path) result

writeFile :: FilePath -> Text -> Result
writeFile path content = Result (Command.WriteFile path content) ()
