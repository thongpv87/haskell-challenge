import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

splitLines :: String->[String]
splitLines [] = []
splitLines lines =
  let (pre, suf) = break isLineTerminator lines
  in
    pre : case suf of
            ('\r':'\n':rest) -> splitLines rest
            ('\r':rest) -> splitLines rest
            ('\n':rest) -> splitLines rest
            _ -> []
isLineTerminator c = c == '\r' || c == '\n'

fixLines input = unlines (splitLines input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
        myFunction = id
