import System.IO

sumFile = sum . map read . words

main = do
  contents <- getLine
  print (sumFile contents)
