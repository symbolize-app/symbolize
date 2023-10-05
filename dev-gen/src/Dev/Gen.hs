module Dev.Gen
  ( test,
    test',
  )
where

import Dev.Gen.Exec qualified as Exec
import Named ((!))
import Relude.Applicative (Applicative (pure))
import Relude.Function (($))
import Relude.Monoid (Semigroup ((<>)))
import Relude.Numeric (Int)
import Relude.String (Text)

test :: Exec.Exec Int
test = pure 3

test' :: Exec.Exec Text
test' = do
  a <- Exec.readFile ! #path "a"
  b <- Exec.readFile ! #path "b"
  pure $ a <> "/" <> b
