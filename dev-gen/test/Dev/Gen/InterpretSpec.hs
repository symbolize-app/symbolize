module Dev.Gen.InterpretSpec
  ( interpret,
    interpretFail,
  )
where

import Data.Typeable (cast)
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.ExecSpec qualified as ExecSpec
import GHC.IO.Exception (IOException (ioe_description))
import Relude.Applicative (empty, pure)
import Relude.Base (Eq, IO, Show, (==))
import Relude.Foldable (null)
import Relude.Monad (Either (Left, Right), Maybe (Just, Nothing), fail, maybe, (>>))
import Relude.String (String)
import Test.Hspec (shouldBe, shouldSatisfy, shouldThrow)

interpret :: (Show a, Eq a) => Exec.Exec a -> [ExecSpec.Result] -> a -> IO ()
interpret e s r = do
  (s', r') <- _interpret e s
  s' `shouldSatisfy` null
  r' `shouldBe` r

interpretFail :: Exec.Exec a -> [ExecSpec.Result] -> String -> IO ()
interpretFail e s f = do
  _interpret e s `shouldThrow` (\(ex :: IOException) -> ex.ioe_description == f)

_interpret :: Exec.Exec a -> [ExecSpec.Result] -> IO ([ExecSpec.Result], a)
_interpret (Exec.Pure x) s = pure (s, x)
_interpret (Exec.Bind x f) s = do
  (s', x') <- _interpret x s
  _interpret (f x') s'
_interpret (Exec.Fail e) _ =
  fail e
_interpret (Exec.Command x) ((ExecSpec.Result y r) : s') = do
  (y', r') <-
    maybe
      ((Left x `shouldBe` Right y) >> empty)
      pure
      (cast (y, r))
  x `shouldBe` y'
  pure (s', r')
_interpret (Exec.Command x) _ =
  (Just x `shouldBe` Nothing) >> empty
