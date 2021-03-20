import Data.Maybe
import Data.Char
import Data.Either

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:xs)
  | null xs = Just x
  | otherwise = safeLast xs

safeInit :: [a]->Maybe [a]
safeInit [] = Nothing
safeInit (x:xs)
  | null xs = Just []
  | otherwise = Just $ [x] ++ fromJust (safeInit xs)

splitWith :: (a->Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs
  | null pre = splitWith f (dropWhile f suf)
  | otherwise = [pre] ++ (splitWith f suf)
  where
    (pre,suf) = span (fmap not f) xs

-- fold lists --
asInt :: [Char] -> Int
asInt [] = 0
asInt ('-':xs) = 0 - asInt xs
asInt xs = foldl (\a b -> a*10 + digitToInt b) 0 xs

asIntEi :: [Char] -> Either String Int
asIntEi [] = Right 0
asIntEi('-':xs) = case asIntEi xs of
                    Right a -> Right (-a)
                    Left err -> Left err
asIntEi xs = foldl acc (Right 0) xs
  where acc (Right a) b
          | isDigit b = Right (a*10 + digitToInt b)
          | otherwise = Left ("non digit " ++ [b])
        acc (Left err) _ = Left err

asInt2 :: [Char] -> Int
--asInt2 xs = fst (foldr (\a b -> (digitToInt a * snd b + fst b, snd b + 1)) (0, 1) xs)
asInt2 xs = foldr (\a b -> b * 10 + digitToInt a) 0 (reverse xs)

reverseList xs = foldl (flip (:)) [] xs

concatList :: [[a]]->[a]
concatList xs = foldr (++) [] xs

myFilter :: (a->Bool)->[a]->[a]
myFilter _ [] = []
myFilter f (x:xs)
  | f x = [x] ++ myFilter f xs
  | otherwise = myFilter f xs

takeWhile1 :: (a->Bool)->[a]->[a]
takeWhile1 _ [] = []
takeWhile1 f (x:xs)
  | f x = [x] ++ takeWhile1 f xs
  | otherwise = []

takeWhile2 :: (a->Bool)->[a]->[a]
takeWhile2 f xs = foldr (\a b -> if f a then a:b else []) [] xs

--groupBy :: (a->a->Bool) -> [a] -> [[a]]
-- TODO

myAny :: [Bool]->Bool
myAny = foldr (&&) True

cycle' :: [a]->[a]
cycle' xs = foldr (:) [] ds
  where ds = loop xs
        loop l = l ++ (loop l)

myWords :: String->[String]
myWords [] = []
myWords (x:xs)
  | isSpace x= myWords xs
  | otherwise =(x:pre):myWords suf
  where (pre, suf) = break isSpace xs

myWords2 :: String->[String]
myWords2 xs = fst (foldr f ([],[]) xs)
  where f x (xs, w)
          | isSpace x = if null w then (xs, w) else (w:xs, [])
          | otherwise = (xs, x:w)

myUnlines :: [String] -> String
myUnlines = foldr (\x y -> x ++ '\n':y) []

dropWhile2 p = foldr f ([],[])
  where f x (ys,xs) = (if p x then ys else x:xs, x:xs)


suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes (x:[]) = [[x]]
suffixes (_:xs) = xs : suffixes xs
