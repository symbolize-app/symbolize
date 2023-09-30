module Main (main) where

import Relude.Function
import Relude.Monad
import Relude.Lifted
import Lib

main :: MonadIO m => m ()
main = do
  putStrLn "hello"
  r <- interpretIO (test)
  print r
  r' <- interpretIO (test')
  print r'
  print $ interpretTest [] test
  print $ interpretTest [TestCommand (ReadFile "a") "q", TestCommand (ReadFile "b") "0"] test'
  print $ interpretTest [TestCommand (ReadFile "a") "q", TestCommand (ReadFile "b") "0", TestCommand (ReadFile "b") "1"] test'
  print $ interpretTest [TestCommand (ReadFile "a") "q", TestCommand (ReadFile "c") "0"] test'
  print $ interpretTest [] test'
  return ()

