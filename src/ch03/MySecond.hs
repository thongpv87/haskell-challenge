import Data.Maybe
safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond (x:xs) = if null xs then Nothing else Just (head xs)

