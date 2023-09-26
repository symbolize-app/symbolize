module Lib
    ( someFunc,
    test,
    test',
    interpretIO
    ) where

import Control.Monad (liftM, ap)

someFunc :: IO ()
someFunc = putStrLn "Hello world"

data Command a where
  Pure :: a -> Command a
  ReadFile :: String -> Command String

data CommandM m a = CommandM (m (Command a))

unwrap (CommandM x) = x

instance Functor (CommandM IO) where
    fmap = liftM

instance Applicative (CommandM IO) where 
    pure = CommandM . pure . Pure
    (<*>) = ap

instance Monad (CommandM IO) where
  (CommandM x') >>= f' =
    CommandM (x' >>= bind)
    where
    f x = unwrap (f' x)
    bind (Pure x) =
      f x
    bind (ReadFile x)  =
      f (x ++ "y")

class Monad m => CommandMM m where
  wrap :: Command a -> m a

instance CommandMM (CommandM IO) where
  wrap x = CommandM (pure x)

test :: CommandMM m => m Int
test = do
  wrap (Pure 3)

test' :: CommandMM m => m String
test' = do
  a <- wrap (ReadFile "a")
  b <- wrap (ReadFile "b")
  return $ a ++ "/" ++ b

interpretIO :: CommandM IO a -> IO a
interpretIO (CommandM x) = do
  (Pure x') <- x
  return x'
