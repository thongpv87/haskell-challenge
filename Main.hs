
module Main where
import Data.List
import XMonad.Core

main :: IO ()
main = do
  dir <- getXMonadDir
  putStrLn dir
  putStrLn "Hello, Haskell!"

