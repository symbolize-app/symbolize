module Dev.Gen.Command
  ( Command (..),
  )
where

import GHC.TypeLits (KnownSymbol, symbolVal)
import Named (NamedF (ArgF), type (:!))
import Relude.Base (Eq, FilePath, Proxy (Proxy), Show, Type)
import Relude.Function ((.))
import Relude.Functor (Identity (Identity))
import Relude.Numeric (Int, Num ((+)))
import Relude.String (Text)
import Text.Show (ShowS, showString, showsPrec)

type Command :: Type -> Type
data Command a where
  ReadFile :: "path" :! FilePath -> Command Text
  WriteFile :: FilePath -> Text -> Command ()

deriving newtype instance (Eq a) => Eq (NamedF Identity a name)

instance (KnownSymbol name, Show a) => Show (NamedF Identity a name) where
  showsPrec :: Int -> NamedF Identity a name -> ShowS
  showsPrec _ (ArgF (Identity a)) =
    showString "! #"
      . showString (symbolVal (Proxy :: Proxy name))
      . showString " "
      . showsPrec (up_prec + 1) a
    where
      up_prec = 9 :: Int

deriving stock instance Eq (Command a)

deriving stock instance Show (Command a)
