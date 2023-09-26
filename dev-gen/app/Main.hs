module Main (main) where

import Lib

main :: IO ()
main = do
  putStrLn "hello"
  r <- interpretIO (test)
  putStrLn (show r)
  r' <- interpretIO (test')
  putStrLn (show r')
  return ()

