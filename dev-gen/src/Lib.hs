module Lib
  ( test,
    test',
    TestCommand (..),
    interpretIO,
    interpretTest,
    Command (..),
  )
where

import Control.Monad (ap, liftM)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Named (NamedF (Arg, ArgF), (!), type (:!))
import Relude.Applicative (Applicative (pure, (<*>)), pass)
import Relude.Base (Eq ((==)), Proxy (Proxy), Show, Type)
import Relude.Function (($), (.))
import Relude.Functor (Functor (fmap), Identity (Identity))
import Relude.Monad (Maybe (Just, Nothing), Monad ((>>=)), MonadIO)
import Relude.Monoid (Semigroup ((<>)))
import Relude.Numeric (Int, Num ((+)))
import Relude.String (Text)
import Text.Show (ShowS, showString, showsPrec)

type Command :: Type -> Type
data Command a where
  ReadFile :: "path" :! Text -> Command Text
  WriteFile :: Text -> Text -> Command ()

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

type TestCommand :: Type
data TestCommand where
  TestCommand :: (Show a) => Command a -> a -> TestCommand

deriving stock instance Show TestCommand

evalTestCommand' :: Command a -> TestCommand -> Maybe a
evalTestCommand' x@(ReadFile _) (TestCommand y@(ReadFile _) r) =
  if x == y then Just r else Nothing
evalTestCommand' _ _ = Nothing

type Exec :: Type -> Type
data Exec a where
  Pure :: a -> Exec a
  Bind :: Exec b -> (b -> Exec a) -> Exec a
  ExecCommand :: Command a -> Exec a

instance Functor Exec where
  fmap :: (a -> b) -> Exec a -> Exec b
  fmap = liftM

instance Applicative Exec where
  pure :: a -> Exec a
  pure = Pure

  (<*>) :: Exec (a -> b) -> Exec a -> Exec b
  (<*>) = ap

instance Monad Exec where
  (>>=) :: Exec a -> (a -> Exec b) -> Exec b
  (>>=) = Bind

test :: Exec Int
test = pure 3

test' :: Exec Text
test' = do
  a <- ExecCommand $ ReadFile ! #path "a"
  b <- ExecCommand $ ReadFile ! #path "b"
  pure $ a <> "/" <> b

interpretIO :: (MonadIO m) => Exec a -> m a
interpretIO (Pure x) = pure x
interpretIO (Bind x f) = interpretIO x >>= interpretIO . f
interpretIO (ExecCommand (ReadFile (Arg x))) = pure $ x <> "x"
interpretIO (ExecCommand (WriteFile _ _)) = pass

interpretTest :: [TestCommand] -> Exec a -> Maybe ([TestCommand], a)
interpretTest s (Pure x) = pure (s, x)
interpretTest s (Bind x f) = do
  (s', x') <- interpretTest s x
  interpretTest s' (f x')
interpretTest (s : s') (ExecCommand x) = do
  x' <- evalTestCommand' x s
  pure (s', x')
interpretTest [] (ExecCommand _) = Nothing
