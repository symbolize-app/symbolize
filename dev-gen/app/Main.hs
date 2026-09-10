import Control.Monad.IO.Unlift (MonadUnliftIO)
import Dev.Gen (gen)
import Dev.Gen.Interpret qualified as Interpret
import Dev.Gen.Vendor.Check qualified as VendorCheck
import Dev.Gen.Vendor.License qualified as VendorLicense
import Numeric (showFFloat)
import Options.Applicative qualified as Options
import Relude.Applicative ((<**>), (<*>))
import Relude.Base (Type, (==), (>))
import Relude.Bool (Bool, not, when, (&&), (||))
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
data MainOptions = MainOptions
  { check :: Bool,
    checkLicenses :: Bool,
    checkVendor :: Bool
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
                        <> Options.help "Check that generated files and vendor closures are up-to-date"
                    )
                  <*> Options.switch
                    ( Options.long "check-licenses"
                        <> Options.help "Audit all third-party code in vendor/ against approved open-source licenses"
                    )
                  <*> Options.switch
                    ( Options.long "check-vendor"
                        <> Options.help "Run vendor closure, duplicate ledger, node.json, and Gleam dependency checks"
                    )
              )
                <**> Options.helper
            )
            (Options.fullDesc <> Options.progDesc "Generate files and verify vendor closures")
      )
  startTime <- getMonotonicTime
  if options.checkLicenses && not options.check
    then do
      ok <- VendorLicense.runLicenseCheck "."
      endTime <- getMonotonicTime
      let timeText = showFFloat (Just 3) (endTime - startTime) ""
      let timeText' = "(" <> toText timeText <> " s)"
      if ok
        then putTextLn $ "OK " <> timeText' <> " ✅"
        else do
          putTextLn $ "Failed " <> timeText' <> " ❌"
          exitFailure
    else
      if options.checkVendor && not options.check
        then do
          ok <- VendorCheck.runVendorCheck "."
          endTime <- getMonotonicTime
          let timeText = showFFloat (Just 3) (endTime - startTime) ""
          let timeText' = "(" <> toText timeText <> " s)"
          if ok
            then putTextLn $ "OK " <> timeText' <> " ✅"
            else do
              putTextLn $ "Failed " <> timeText' <> " ❌"
              exitFailure
        else
          if options.check
            then do
              (Sum t, ()) <- Interpret.interpret gen Interpret.Check
              putTextLn $ "Files skipped: " <> show t
              vendorOk <- VendorCheck.runVendorCheck "."
              licenseOk <- VendorLicense.runLicenseCheck "."
              endTime <- getMonotonicTime
              let timeText = showFFloat (Just 3) (endTime - startTime) ""
              let timeText' = "(" <> toText timeText <> " s)"
              if t == 0 && vendorOk && licenseOk
                then putTextLn $ "OK " <> timeText' <> " ✅"
                else do
                  putTextLn $ "Failed " <> timeText' <> " ❌"
                  exitFailure
            else do
              (Sum t, ()) <- Interpret.interpret gen Interpret.Generate
              endTime <- getMonotonicTime
              let timeText = showFFloat (Just 3) (endTime - startTime) ""
              let timeText' = "(" <> toText timeText <> " s)"
              putTextLn $ "Files written: " <> show t
              putTextLn $ "Done " <> timeText' <> " ✅"
