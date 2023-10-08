module Dev.Gen.InterpretSpec
  ( interpret,
  )
where

import Data.Typeable (cast)
import Dev.Gen.Command qualified as Command
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.ExecSpec qualified as ExecSpec
import Relude (Monad ((>>=)))
import Relude.Applicative (Alternative (empty), Applicative (pure))
import Relude.Base (Eq ((==)), Typeable)
import Relude.Monad (Maybe (Nothing))

interpret :: [ExecSpec.Result] -> Exec.Exec a -> Maybe ([ExecSpec.Result], a)
interpret s (Exec.Pure x) = pure (s, x)
interpret s (Exec.Bind x f) = do
  (s', x') <- interpret s x
  interpret s' (f x')
interpret _ (Exec.Fail _) = do
  Nothing
interpret (s : s') (Exec.Command x) = do
  x' <- interpretExec x s
  pure (s', x')
interpret [] (Exec.Command _) = Nothing

interpretExec :: (Typeable a) => Command.Command a -> ExecSpec.Result -> Maybe a
interpretExec x (ExecSpec.Result y r) =
  cast (y, r) >>= \(y', r') -> if x == y' then pure r' else empty
