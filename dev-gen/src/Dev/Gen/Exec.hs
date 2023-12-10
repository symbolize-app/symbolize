module Dev.Gen.Exec
  ( Exec (..),
    async,
    await,
    await3,
    await_,
    readJSON,
    readLines,
    readYAML,
    writeJSON,
    writeLines,
    writeYAML,
  )
where

import Control.Monad (MonadFail, ap, liftM, (>>))
import Data.Aeson qualified as Aeson
import Data.Vector (Vector)
import Dev.Gen.Command qualified as Command
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.FilePath (FilePath)
import Relude.Applicative (Applicative, liftA3, pass, pure, (<*>))
import Relude.Base (Eq, Show, Type, Typeable)
import Relude.Function (($), (.))
import Relude.Functor (Functor, fmap, (<$>))
import Relude.Monad (Monad, fail, (>>=))
import Relude.String (String, Text)

type Exec :: Type -> Type
data Exec a where
  Pure :: a -> Exec a
  Bind :: Exec b -> (b -> Exec a) -> Exec a
  Concurrently :: Exec a -> Exec b -> Exec (a, b)
  Fail :: String -> Exec a
  Command :: (Typeable a) => Command.Command a -> Exec a

instance Functor Exec where
  fmap :: (a -> b) -> Exec a -> Exec b
  fmap = liftM

instance Applicative Exec where
  pure :: a -> Exec a
  pure = Pure

  (<*>) :: Exec (a -> b) -> Exec a -> Exec b
  (<*>) = ap

instance Monad Exec where
  (>>=) :: Exec a -> (a -> Exec b) -> Exec b
  (>>=) = Bind

instance MonadFail Exec where
  fail :: String -> Exec a
  fail = Fail

readJSON ::
  (Aeson.FromJSON a, Eq a, Show a, Typeable a) =>
  FilePath ->
  Exec a
readJSON = _command1 (Command.ReadJSON FileFormat.JSON)

writeJSON ::
  (Aeson.ToJSON b, Eq b, Show b, Typeable b) =>
  FilePath ->
  b ->
  Exec ()
writeJSON = _command2 (Command.WriteJSON FileFormat.JSON)

readYAML ::
  (Aeson.FromJSON a, Eq a, Show a, Typeable a) =>
  FilePath ->
  Exec a
readYAML = _command1 (Command.ReadJSON FileFormat.YAML)

writeYAML ::
  (Aeson.ToJSON b, Eq b, Show b, Typeable b) =>
  FilePath ->
  b ->
  Exec ()
writeYAML = _command2 (Command.WriteJSON FileFormat.YAML)

readLines :: FilePath -> Exec (Vector Text)
readLines = _command1 Command.ReadLines

writeLines :: FilePath -> Vector Text -> Exec ()
writeLines = _command2 Command.WriteLines

_command1 :: (Typeable a) => (t1 -> Command.Command a) -> t1 -> Exec a
_command1 command b = Command (command b)

_command2 ::
  (Typeable a) =>
  (t1 -> t2 -> Command.Command a) ->
  t1 ->
  t2 ->
  Exec a
_command2 command b c = Command (command b c)

type ExecConcurrently :: Type -> Type
newtype ExecConcurrently a where
  ExecConcurrently :: Exec a -> ExecConcurrently a

deriving stock instance Functor ExecConcurrently

instance Applicative ExecConcurrently where
  pure :: a -> ExecConcurrently a
  pure = ExecConcurrently . pure

  (<*>) ::
    ExecConcurrently (a -> b) ->
    ExecConcurrently a ->
    ExecConcurrently b
  (<*>) (ExecConcurrently f) (ExecConcurrently x) =
    ExecConcurrently $ (\(f', x') -> f' x') <$> Concurrently f x

await :: ExecConcurrently a -> Exec a
await (ExecConcurrently x) = x

await3 ::
  ExecConcurrently a ->
  ExecConcurrently b ->
  ExecConcurrently c ->
  Exec (a, b, c)
await3 a b c = await $ liftA3 (,,) a b c

await_ :: ExecConcurrently a -> Exec ()
await_ x = await x >> pass

async :: Exec a -> ExecConcurrently a
async = ExecConcurrently
