import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

firstWords input = unlines (fmap (head.words) (lines input))

firstWord :: [String]->[String]
firstWord [] = []
firstWord (x:xs) = [head (words x)] ++ (firstWord xs)

fw input = unlines (firstWord (lines input))

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
        myFunction = unlines.firstWord.lines
