module Lib
    ( test
    , test'
    , interpretIO
    ) where

import Control.Monad (liftM, ap)

data Command a where
  ReadFile :: String -> Command String
  WriteFile :: String -> String -> Command ()

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

test' :: Exec String
test' = do
  a <- ExecCommand $ ReadFile "a"
  b <- ExecCommand $ ReadFile "b"
  return $ a ++ "/" ++ b

interpretIO :: Exec a -> IO a
interpretIO (Pure x) = return x
interpretIO (Bind x f) = interpretIO x >>= (interpretIO . f)
interpretIO (ExecCommand (ReadFile x)) = return $ x ++ "x"
interpretIO (ExecCommand (WriteFile _ _)) = return ()
