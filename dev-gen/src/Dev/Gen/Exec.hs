module Dev.Gen.Exec
  ( Exec (..),
    readFile,
    writeFile,
  )
where

import Control.Monad (MonadFail, ap, liftM)
import Data.Aeson qualified as Aeson
import Dev.Gen.Command qualified as Command
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.FilePath (FilePath)
import Relude.Applicative (Applicative (pure, (<*>)))
import Relude.Base (Eq, Show, Type, Typeable)
import Relude.Functor (Functor (fmap))
import Relude.Monad (Monad ((>>=)), MonadFail (fail))
import Relude.String (String)

type Exec :: Type -> Type
data Exec a where
  Pure :: a -> Exec a
  Bind :: Exec b -> (b -> Exec a) -> Exec a
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

readFile :: (Aeson.FromJSON a, Eq a, Show a, Typeable a) => FilePath -> FileFormat.FileFormat -> Exec a
readFile = _command2 Command.ReadFile

writeFile :: (Aeson.ToJSON b, Eq b, Show b, Typeable b) => FilePath -> FileFormat.FileFormat -> b -> Exec ()
writeFile = _command3 Command.WriteFile

_command2 :: (Typeable a) => (t1 -> t2 -> Command.Command a) -> t1 -> t2 -> Exec a
_command2 command b c = Command (command b c)

_command3 :: (Typeable a) => (t1 -> t2 -> t3 -> Command.Command a) -> t1 -> t2 -> t3 -> Exec a
_command3 command b c d = Command (command b c d)
