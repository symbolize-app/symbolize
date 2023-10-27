import Dev.Gen (gen)
import Dev.Gen.Interpret qualified as Interpret
import Numeric (showGFloatAlt)
import Relude.Function (($))
import Relude.Monad (Maybe (Just))
import Relude.Monoid ((<>))
import Relude.Numeric ((-))
import Relude.Print (putTextLn)
import Relude.String (show, toText)
import UnliftIO (MonadUnliftIO, getMonotonicTime)

main :: (MonadUnliftIO m) => m ()
main = do
  startTime <- getMonotonicTime
  (t, ()) <- Interpret.interpret gen
  endTime <- getMonotonicTime
  putTextLn $ "Files written: " <> show t
  putTextLn $ "Done (" <> toText (showGFloatAlt (Just 3) (endTime - startTime) "") <> " s) ✅"
