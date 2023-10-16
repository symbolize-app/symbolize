import Dev.Gen (gen)
import Dev.Gen.Interpret qualified as Interpret
import Relude.Monad (MonadIO)
import Relude.Print (putTextLn)

main :: (MonadIO m) => m ()
main = do
  Interpret.interpret gen
  putTextLn "Done ✅"
