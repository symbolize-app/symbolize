module Dev.Gen.InterpretSpec
  ( interpret,
  )
where

import Data.Typeable (cast)
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.ExecSpec qualified as ExecSpec
import Relude (MonadFail (fail))
import Relude.Applicative (Applicative (pure))
import Relude.Base (Eq ((==)))
import Relude.Monad (Either, maybe)
import Relude.String (String)

interpret :: [ExecSpec.Result] -> Exec.Exec a -> Either String ([ExecSpec.Result], a)
interpret s (Exec.Pure x) = pure (s, x)
interpret s (Exec.Bind x f) = do
  (s', x') <- interpret s x
  interpret s' (f x')
interpret _ (Exec.Fail e) =
  fail e
interpret ((ExecSpec.Result y r) : s') (Exec.Command x) = do
  (y', r') <- maybe (fail "Command wrong type") pure (cast (y, r))
  x' <- if x == y' then pure r' else fail "Command not equal"
  pure (s', x')
interpret [] (Exec.Command _) =
  fail "No commands left"
