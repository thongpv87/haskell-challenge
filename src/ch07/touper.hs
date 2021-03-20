import Data.Char(toUpper)

main = do
  inpStr <- readFile "input.txt"
  putStrLn $ map toUpper inpStr
  putStrLn inpStr
