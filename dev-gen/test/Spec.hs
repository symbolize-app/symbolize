import Dev.Gen (test, test')
import Dev.Gen.ExecSpec qualified as ExecSpec
import Dev.Gen.InterpretSpec (interpret)
import Named ((!))
import Relude.Applicative (pass)
import Relude.Function (($))
import Relude.Lifted (print)
import Relude.Monad (MonadIO)

main :: (MonadIO m) => m ()
main = do
  print $ interpret [] test
  print $
    interpret
      [ ExecSpec.readFile ! #path "a" ! #result "q",
        ExecSpec.readFile ! #path "b" ! #result "0"
      ]
      test'
  print $
    interpret
      [ ExecSpec.readFile ! #path "a" ! #result "q",
        ExecSpec.readFile ! #path "b" ! #result "0",
        ExecSpec.readFile ! #path "b" ! #result "1"
      ]
      test'
  print $
    interpret
      [ ExecSpec.readFile ! #path "a" ! #result "q",
        ExecSpec.readFile ! #path "c" ! #result "0"
      ]
      test'
  print $ interpret [] test'
  pass
