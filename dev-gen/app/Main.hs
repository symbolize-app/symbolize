import Dev.Gen (test, test')
import Dev.Gen.Interpret (interpret)
import Relude.Applicative (pass)
import Relude.Lifted (print, putStrLn)
import Relude.Monad (MonadIO)

main :: (MonadIO m) => m ()
main = do
  putStrLn "hello"
  r <- interpret test
  print r
  r' <- interpret test'
  print r'
  pass
