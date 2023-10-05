module Dev.Gen.Exec
  ( Exec (..),
    readFile,
    writeFile,
  )
where

import Control.Monad (ap, liftM)
import Dev.Gen.Command qualified as Command
import Named (type (:!))
import Relude.Applicative (Applicative (pure, (<*>)))
import Relude.Base (FilePath, Type)
import Relude.Function (($))
import Relude.Functor (Functor (fmap))
import Relude.Monad (Monad ((>>=)))
import Relude.String (Text)

type Exec :: Type -> Type
data Exec a where
  Pure :: a -> Exec a
  Bind :: Exec b -> (b -> Exec a) -> Exec a
  Command :: Command.Command a -> Exec a

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

readFile :: "path" :! FilePath -> Exec Text
readFile path = Command $ Command.ReadFile path

writeFile :: FilePath -> Text -> Exec ()
writeFile path content = Command $ Command.WriteFile path content
