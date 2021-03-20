import System.IO
str2msg :: String -> String
str2msg input = "Data: " ++ input

str2act :: String -> IO ()
str2act = putStrLn . str2msg

numbers :: [Int]
numbers = [1..10]

main = do
  str2act "Start programs"
  mapM_ (str2act . show) numbers
  mode <- return stdin >>= hGetBuffering
  putStrLn (show mode)
  str2act "Done"
