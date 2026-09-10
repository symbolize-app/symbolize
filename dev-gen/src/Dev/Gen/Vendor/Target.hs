module Dev.Gen.Vendor.Target
  ( deriveAliases,
    genVendorBuck,
  )
where

import Data.List qualified as List
import Data.Text qualified as Text
import Data.Tuple (fst)
import Data.Vector (Vector)
import Relude.Base (Eq, (==))
import Relude.Container (fromList)
import Relude.Foldable (foldMap', toList)
import Relude.Function (($), (.))
import Relude.Monad ((>>=))
import Relude.Monoid ((<>))
import Relude.String (Text)

deriveAliases :: Vector (Text, Text) -> Vector (Text, Text)
deriveAliases rawTargets =
  let expanded =
        toList rawTargets >>= \(name, actual) ->
          if Text.isInfixOf "-" name
            then [(name, actual), (Text.replace "-" "_" name, actual)]
            else [(name, actual)]
      uniqueAliases =
        List.sortOn fst
          . List.nubBy (\a b -> fst a == fst b)
          $ expanded
   in fromList uniqueAliases

genVendorBuck :: Vector (Text, Text) -> Vector Text
genVendorBuck rawTargets =
  let aliases = deriveAliases rawTargets
      header = ["load(\"//:rules.bzl\", \"alias\")"]
      formatAlias (name, actual) =
        [ "",
          "alias(",
          "    name = \"" <> name <> "\",",
          "    actual = \"" <> actual <> "\",",
          ")"
        ]
   in header <> foldMap' formatAlias aliases
