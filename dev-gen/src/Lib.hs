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

newtype CommandM m a = CommandM (m (Command a))

unwrap (CommandM x) = x

instance Unlift m => Functor (CommandM m) where
    fmap = liftM

instance Unlift m => Applicative (CommandM m) where 
    pure = CommandM . pure . Pure
    (<*>) = ap

instance Unlift m => Monad (CommandM m) where
  (CommandM x') >>= f' =
    CommandM (x' >>= bind)
    where
    f x = unwrap (f' x)
    bind x =
      unlift x >>= f

class Monad m => Unlift m where
  unlift :: Command a -> m a

instance Unlift IO where
  unlift (Pure x) =
    return x
  unlift (ReadFile x)  =
    return (x ++ "y")

class Monad m => CommandMM m where
  wrap :: Command a -> m a

instance Unlift m => CommandMM (CommandM m) where
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
interpretIO (CommandM x) = x >>= unlift
