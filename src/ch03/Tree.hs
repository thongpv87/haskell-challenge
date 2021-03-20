import Data.Maybe
data Tree a = Empty | Node a (Tree a) (Tree a)
            deriving Show

data AltTree a = AltNode (Maybe a) (AltTree a) (AltTree a)

depth :: Tree a -> Int
depth Empty = 0
depth (Node a left right) = 1 + max (depth left) (depth right)
