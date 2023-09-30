module Lib
    ( test
    , test'
    , TestCommand(..)
    , interpretIO
    , interpretTest
    , Command(..)
    ) where

import Relude.Base
import Relude.Monad
import Relude.Numeric
import Relude.Functor
import Relude.Applicative
import Relude.Function
import Relude.String
import Relude.Monoid
import Control.Monad (liftM, ap)

data Command a where
  ReadFile :: Text -> Command Text
  WriteFile :: Text -> Text -> Command ()

deriving instance Eq (Command a)
deriving instance Show (Command a)

data TestCommand where
  TestCommand :: (Show a) => Command a -> a -> TestCommand

deriving instance Show TestCommand

evalTestCommand' :: Command a -> TestCommand -> Maybe a
evalTestCommand' x@(ReadFile _) (TestCommand y@(ReadFile _) r) = if x == y then Just r else Nothing
evalTestCommand' _ _ = Nothing

data Exec a where
  Pure :: a -> Exec a
  Bind :: Exec b -> (b -> Exec a) -> Exec a
  ExecCommand :: Command a -> Exec a

instance Functor Exec where
    fmap = liftM

instance Applicative Exec where 
    pure = Pure
    (<*>) = ap

instance Monad Exec where
  (>>=) = Bind

test :: Exec Int
test = do
  return 3

test' :: Exec Text
test' = do
  a <- ExecCommand $ ReadFile "a"
  b <- ExecCommand $ ReadFile "b"
  return $ a <> "/" <> b

interpretIO :: MonadIO m => Exec a -> m a
interpretIO (Pure x) = return x
interpretIO (Bind x f) = interpretIO x >>= (interpretIO . f)
interpretIO (ExecCommand (ReadFile x)) = return $ x <> "x"
interpretIO (ExecCommand (WriteFile _ _)) = return ()

interpretTest :: [TestCommand] -> Exec a -> Maybe ([TestCommand], a)
interpretTest s (Pure x) = return (s, x)
interpretTest s (Bind x f) = do
  (s', x') <- interpretTest s x
  interpretTest s' (f x')
interpretTest (s:s') (ExecCommand x) = do
  x' <- evalTestCommand' x s
  return (s', x')
interpretTest [] (ExecCommand _) = Nothing
