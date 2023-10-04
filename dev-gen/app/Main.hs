module Main (main) where

import Lib
  ( Command (ReadFile),
    TestCommand (TestCommand),
    interpretIO,
    interpretTest,
    test,
    test',
  )
import Named ((!))
import Relude.Applicative (pass)
import Relude.Function (($))
import Relude.Lifted (print, putStrLn)
import Relude.Monad (MonadIO)

main :: (MonadIO m) => m ()
main = do
  putStrLn "hello"
  r <- interpretIO test
  print r
  r' <- interpretIO test'
  print r'
  print $ interpretTest [] test
  print $
    interpretTest
      [ TestCommand (ReadFile ! #path "a") "q",
        TestCommand (ReadFile ! #path "b") "0"
      ]
      test'
  print $
    interpretTest
      [ TestCommand (ReadFile ! #path "a") "q",
        TestCommand (ReadFile ! #path "b") "0",
        TestCommand (ReadFile ! #path "b") "1"
      ]
      test'
  print $
    interpretTest
      [ TestCommand (ReadFile ! #path "a") "q",
        TestCommand (ReadFile ! #path "c") "0"
      ]
      test'
  print $ interpretTest [] test'
  pass
