import Data.Char
import Data.Functor.Contravariant
import Data.Bifunctor
import Data.List

digpow :: Integer -> Integer -> Integer
digpow n p =
  let
      s =  sum $ zipWith (^) (fmap (toInteger.digitToInt) $ show n) [p..]
  in if s `mod` n == 0 then s `div` n else -1


sortArray :: [Int] -> [Int]
sortArray xs = let
  ps = sort . filter odd $ xs
  step x (xss, yss) = ((if odd x then head yss else x):xss, tail yss)
  in fst $ foldr step ([], ps) xs

main = do
  putStrLn $ show $ sortArray [5, 3, 2, 8, 1, 4]
