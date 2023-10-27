{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Dev.Gen (gen)
import Dev.Gen.Interpret qualified as Interpret
import Numeric (showFFloat)
import Options.Applicative qualified as Options
import Relude.Applicative ((<**>))
import Relude.Base (Type, (>))
import Relude.Bool (Bool, when)
import Relude.Function (($))
import Relude.Functor ((<$>))
import Relude.Lifted (exitFailure)
import Relude.Monad (Maybe (Just), liftIO)
import Relude.Monoid (Sum (Sum), (<>))
import Relude.Numeric ((-))
import Relude.Print (putTextLn)
import Relude.String (show, toText)
import UnliftIO.IO (getMonotonicTime)

type MainOptions :: Type
newtype MainOptions = MainOptions
  { check :: Bool
  }

main :: (MonadUnliftIO m) => m ()
main = do
  options <-
    liftIO
      ( Options.execParser $
          Options.info
            ( ( MainOptions
                  <$> Options.switch
                    ( Options.long "check"
                    )
              )
                <**> Options.helper
            )
            (Options.fullDesc <> Options.progDesc "Generate files")
      )
  let mode = if options.check then Interpret.Check else Interpret.Generate
  startTime <- getMonotonicTime
  (Sum t, ()) <- Interpret.interpret gen mode
  endTime <- getMonotonicTime
  let timeText = "(" <> toText (showFFloat (Just 3) (endTime - startTime) "") <> " s)"
  case mode of
    Interpret.Generate -> do
      putTextLn $ "Files written: " <> show t
      putTextLn $ "Done " <> timeText <> " ✅"
    Interpret.Check -> do
      putTextLn $ "Files skipped: " <> show t
      when (t > 0) $ do
        putTextLn $ "Failed " <> timeText <> " ❌"
        exitFailure
      putTextLn $ "OK " <> timeText <> " ✅"
