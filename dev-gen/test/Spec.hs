import Dev.Gen (gen)
import Dev.Gen.ExecSpec qualified as ExecSpec
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.InterpretSpec (interpret)
import Relude.Applicative (pass)
import Relude.Function (($))
import Relude.Lifted (print)
import Relude.Monad (MonadIO)

main :: (MonadIO m) => m ()
main = do
  print $
    interpret
      [ ExecSpec.readFile "a" FileFormat.YAML "q",
        ExecSpec.readFile "b" FileFormat.YAML "0"
      ]
      gen
  print $
    interpret
      [ ExecSpec.readFile "a" FileFormat.YAML "q",
        ExecSpec.readFile "b" FileFormat.YAML "0",
        ExecSpec.readFile "b" FileFormat.YAML "1"
      ]
      gen
  print $
    interpret
      [ ExecSpec.readFile "a" FileFormat.YAML "q",
        ExecSpec.readFile "c" FileFormat.YAML "0"
      ]
      gen
  print $ interpret [] gen
  pass
