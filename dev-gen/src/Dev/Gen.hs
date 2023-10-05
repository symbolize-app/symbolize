module Dev.Gen
  ( test,
    test',
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Vector qualified as Vector
import Dev.Gen.Exec qualified as Exec
import Dev.Gen.FileFormat qualified as FileFormat
import Relude.Applicative (Applicative (pure))
import Relude.Function (($), (.))
import Relude.Numeric (Int)

test :: Exec.Exec Int
test = pure 3

test' :: Exec.Exec Aeson.Value
test' = do
  a <- Exec.readFile "../pnpm-workspace.yaml" FileFormat.YAML
  b <- Exec.readFile "../package.json" FileFormat.YAML
  Exec.writeFile "../tmp/x.yaml" FileFormat.YAML
    . Aeson.Object
    $ Aeson.KeyMap.fromList
      [ ("key", Aeson.String "value")
      ]
  pure . Aeson.Array . Vector.fromList $ [a, b]
