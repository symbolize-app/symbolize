import Control.Monad.IO.Unlift (MonadUnliftIO)
import Dev.Gen (gen)
import Dev.Gen.Interpret qualified as Interpret
import Numeric (showFFloat)
import Relude.Function (($))
import Relude.Monad (Maybe (Just))
import Relude.Monoid (Sum (Sum), (<>))
import Relude.Numeric ((-))
import Relude.Print (putTextLn)
import Relude.String (show, toText)
import UnliftIO.IO (getMonotonicTime)

main :: (MonadUnliftIO m) => m ()
main = do
  startTime <- getMonotonicTime
  (Sum t, ()) <- Interpret.interpret gen
  endTime <- getMonotonicTime
  putTextLn $ "Files written: " <> show t
  putTextLn $ "Done (" <> toText (showFFloat (Just 3) (endTime - startTime) "") <> " s) ✅"
