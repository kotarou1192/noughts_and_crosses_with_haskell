module Main where

import qualified TmpLib (tmpFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  TmpLib.tmpFunc
