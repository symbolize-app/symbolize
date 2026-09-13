module Dev.Gen.Package (isRustService) where

import Data.Text qualified as Text
import Relude.Bool (Bool)
import Relude.String (Text)

isRustService :: Text -> Bool
isRustService = Text.isPrefixOf "svc-"
