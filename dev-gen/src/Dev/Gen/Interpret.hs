module Dev.Gen.Interpret
  ( interpret,
  )
where

import Dev.Gen.Command qualified as Command
import Dev.Gen.Exec qualified as Exec
import Named (NamedF (Arg))
import Relude.Applicative (Applicative (pure), pass)
import Relude.Function (($), (.))
import Relude.Monad (Monad ((>>=)), MonadIO)
import Relude.Monoid (Semigroup ((<>)))
import Relude.String (fromString)

interpret :: (MonadIO m) => Exec.Exec a -> m a
interpret (Exec.Pure x) = pure x
interpret (Exec.Bind x f) = interpret x >>= interpret . f
interpret (Exec.Command (Command.ReadFile (Arg x))) = pure $ fromString x <> "x"
interpret (Exec.Command (Command.WriteFile _ _)) = pass
