import Dev.Gen (gen)
import Dev.Gen.Interpret qualified as Interpret
import Relude.Applicative (pass)
import Relude.Lifted (print, putStrLn)
import Relude.Monad (MonadIO)

main :: (MonadIO m) => m ()
main = do
  putStrLn "hello"
  r <- Interpret.interpret gen
  print r
  pass
