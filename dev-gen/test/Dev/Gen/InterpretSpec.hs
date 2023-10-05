module Dev.Gen.InterpretSpec
  ( interpret,
  )
where

import Dev.Gen.Command qualified as Command
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.ExecSpec qualified as ExecSpec
import Relude.Applicative (Applicative (pure))
import Relude.Base (Eq ((==)))
import Relude.Monad (Maybe (Just, Nothing))

interpret :: [ExecSpec.Result] -> Exec.Exec a -> Maybe ([ExecSpec.Result], a)
interpret s (Exec.Pure x) = pure (s, x)
interpret s (Exec.Bind x f) = do
  (s', x') <- interpret s x
  interpret s' (f x')
interpret (s : s') (Exec.Command x) = do
  x' <- interpretExec x s
  pure (s', x')
interpret [] (Exec.Command _) = Nothing

interpretExec :: Command.Command a -> ExecSpec.Result -> Maybe a
interpretExec x@(Command.ReadFile _) (ExecSpec.Result y@(Command.ReadFile _) r) =
  if x == y then Just r else Nothing
interpretExec _ _ = Nothing
