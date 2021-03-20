-- myDrop n xs = if n<= 0 || null xs then xs else myDrop (n-1) (tail xs)
myDrop :: Int->[a]->[a]
myDrop 0 xs = xs
myDrop n xs = myDrop (n-1) (tail xs)

lastButOne :: [a]->a
lastButOne (x:xs) = if length xs == 1 then x else lastButOne xs

main = interact wordCount
  where wordCount input = show (length input) ++ "\n"
