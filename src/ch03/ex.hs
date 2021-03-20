myLength :: [a]->Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myAvg [] = 0
myAvg xs = fromIntegral (sum xs)/fromIntegral (myLength xs)
           where
             sum [] = 0
             sum (x:xs) = x + sum xs

reverseList :: [a]->[a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

makePalindromeList :: [a]->[a]
makePalindromeList xs = (xs ++ reverseList xs)

palindrome :: Eq a => [a]->Bool
palindrome xs = xs == reverseList xs

myFilter :: (a->Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
  | f x = x:myFilter f xs
  | otherwise = myFilter f xs

mySort :: (a->a->Ordering) -> [a]->[a]
mySort _ [] = []
mySort f (x:xs) = (mySort f le) ++ [x] ++ (mySort f gt)
  where le = myFilter (\a->f a x == LT || f a x == EQ) xs
        gt = myFilter (\a-> f a x == GT) xs

sortLists :: [[a]]->[[a]]
sortLists xs = mySort (\a b->compare (length a) (length b)) xs

intersperse :: a->[[a]]->[a]
intersperse _ [] = []
intersperse _ (x:[]) = x
intersperse a (x:xs) = x ++ [a] ++ intersperse a xs

data Point = Point {
  x::Double
  , y::Double
  } deriving Show

data Direction = L | H | R
  deriving Show

direction :: Point -> Point -> Point -> Direction
direction a b c
  | z > 0 = L
  | z == 0 = H
  | z < 0 = R
  where
    z = xab * yac - yab * xac
    xab = x b - x a
    yac = y c - y a
    yab = y b - y a
    xac = x c - x a

points = [(Point 0 0), (Point 0 1), (Point 1 1), (Point (-1) 5), (Point 3 2), (Point 1 4), (Point (-4) 7)]
scanDirection :: [Point]->[Direction]
scanDirection xs | length xs < 3 = []
scanDirection (a:b:c:xs) = direction a b c : (scanDirection (b:c:xs))





