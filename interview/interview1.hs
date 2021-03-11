{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Main where
import Data.List


mySum :: [Int] -> Int
mySum (x:xs) = x + sum xs
mySum [] = 0


myReverse :: [a] -> [a]
myReverse (x:xs) = myReverse xs ++ [x]
myReverse [] = []

myReverse2 :: [a]->[a]
myReverse2 = foldl (flip (:)) []

myReverse3 :: [a]->[a]
myReverse3 = foldr f []
  where f x ys = ys ++ [x]

myFilter :: (a->Bool)->[a]->[a]
myFilter f (x:xs)
  | f x = x : myFilter f xs
  | otherwise = myFilter f xs
myFilter _ [] = []

myFilter2 :: (a->Bool)->[a]->[a]
myFilter2 f xs = foldr (\x ys-> if f x then x:ys else ys) [] xs

myTakeWhile :: (a->Bool)->[a]->[a]
myTakeWhile f xs = foldr (\x ys -> if f x then x:ys else []) [] xs

myFizzBuzz :: [Int]->String
myFizzBuzz xs = unlines $ map f xs
  where f x
          | x `mod` 3 == 0 = "Fizz!"
          | x `mod` 3 == 1 = "Buzz!"
          | otherwise = show x

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n (x:xs) = fmap (x:) (combinations (n-1) xs) <> combinations n xs
combinations _ [] = []

main :: IO ()
main = do
  -- putStrLn $ show $ myReverse3 [1..10]
  -- putStrLn $ show $ myTakeWhile (<5) [1..10]
  putStrLn $ show $ combinations 3 [1..5]
  putStrLn $ myFizzBuzz [1..5]

