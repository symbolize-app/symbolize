module Dev.Gen.Interpret
  ( interpret,
  )
where

import Data.Aeson qualified as Aeson
import Data.Yaml qualified as Yaml
import Dev.Gen.Command qualified as Command
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.FileFormat qualified as FileFormat
import Relude.Applicative (Applicative (pure))
import Relude.File (readFileBS, readFileLBS, writeFileBS, writeFileLBS)
import Relude.Function (($), (.))
import Relude.Monad (Either (Left, Right), Monad ((>>=)), MonadFail (fail), MonadIO (liftIO))
import Relude.String (show, toString)

interpret :: (MonadIO m) => Exec.Exec a -> m a
interpret (Exec.Pure x) =
  pure x
interpret (Exec.Bind x f) =
  interpret x >>= interpret . f
interpret (Exec.Fail s) =
  liftIO $ fail s
interpret (Exec.Command (Command.ReadFile filePath FileFormat.YAML)) = do
  bytes <- readFileBS (toString filePath)
  case Yaml.decodeEither' bytes of
    (Left exception) -> liftIO (fail (show exception))
    (Right value) -> pure value
interpret (Exec.Command (Command.ReadFile filePath FileFormat.JSON)) = do
  bytes <- readFileLBS (toString filePath)
  case Aeson.eitherDecode bytes of
    (Left error) -> liftIO (fail error)
    (Right value) -> pure value
interpret (Exec.Command (Command.WriteFile filePath FileFormat.YAML value)) =
  writeFileBS (toString filePath) $ Yaml.encode value
interpret (Exec.Command (Command.WriteFile filePath FileFormat.JSON value)) =
  writeFileLBS (toString filePath) $ Aeson.encode value
