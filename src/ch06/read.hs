data JValue = JString String
type JSONError = String
main = do
  putStrLn "Enter a double: "
  s <- getLine
  let d = (read s)::Double
  putStrLn ("Twice of " ++ s ++ " is: " ++ show (2 * d))

