import Dev.Gen (gen)
import Dev.Gen.Interpret qualified as Interpret
import Relude.Print (putTextLn)
import UnliftIO (MonadUnliftIO)

main :: (MonadUnliftIO m) => m ()
main = do
  Interpret.interpret gen
  putTextLn "Done ✅"
