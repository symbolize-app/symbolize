module Dev.Gen.Package (isCargoService) where

import Data.Text qualified as Text
import Relude.Bool (Bool)
import Relude.String (Text)

isCargoService :: Text -> Bool
isCargoService = Text.isPrefixOf "svc-"
