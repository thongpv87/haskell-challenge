module Main where

import Control.Monad
import Data.List as L
import qualified Data.Sequence as S
import System.Random as R

data Customer = Yellow | Red | Blue deriving (Show)

data CustomerEntry = CustomerEntry
  { arrivingInterval :: Float,
    processingTime :: Float
  }
  deriving (Show)

-- Get the second since the last customer arrive from probability arriving of customer
intervalTime :: Float -> Float
intervalTime pArrive = -1 * alpha * log (1 - pArrive)
  where
    alpha = 200

-- Get processing time of customer
processTime :: Customer -> Float -> Float
processTime c x = p * x ^ (alpha -1) * (1 - x) ^ (beta - 1)
  where
    p = 200
    (alpha, beta) = case c of
      Yellow -> (2, 5)
      Red -> (2, 2)
      Blue -> (5, 1)

-- Generate infinite list of RandomGen
generators :: (RandomGen g) => g -> [g]
generators = unfoldr (Just . R.split)

-- Generate infinite list of list of random float
rolls :: (RandomGen g) => g -> [[Float]]
rolls g = fmap (randomRs (0 :: Float, 1 :: Float)) $ generators g

-- Generate customer queue, each entry contains their arriving interval since last customer
-- and processing time
queue :: (RandomGen g) => g -> Customer -> [CustomerEntry]
queue g c =
  let (ps1 : ps2 : xs : _) = rolls g
      pArrives = fst $ unzip $ filter (\(pArrive, pJoin) -> pJoin <= pArrive) $ zip ps1 ps2
      arriveIntervals = fmap intervalTime pArrives
      processTimes = fmap (processTime c) xs
   in fmap (\(x, y) -> CustomerEntry x y) $ zip arriveIntervals processTimes

toAbsoluteTime :: [CustomerEntry] -> [(Float, Float)]
toAbsoluteTime cs = drop 1 $ scanl f (0.0, 0.0) cs
  where
    f (x, y) (CustomerEntry a b) = (x + a, max (x + a) y + b)

-- calculate avg and maximum customer waiting times
task1 :: [CustomerEntry] -> (Float, Float)
task1 cs =
  let arrivals = toAbsoluteTime cs
      f2 :: (Float, Float) -> (Float, Float) -> Float
      f2 (_, y1) (x2, _) = if y1 > x2 then y1 - x2 else 0
      waitingTimes = zipWith f2 arrivals (tail arrivals)
      f3 :: Float -> (Int, Float, Float) -> (Int, Float, Float)
      f3 x (l, s, m) = (l + 1, s + x, max x m)
      (len, sum, maxx) = foldr f3 (0, 0, 0) waitingTimes
   in (sum / fromIntegral len, maxx)

-- Customer who is serving by teller is excluded from queue length
task2 :: [CustomerEntry] -> (Float, Int)
task2 cs =
  let arrivals = toAbsoluteTime cs
      f (maxWaiting, waitingQ, sumLen) c = (maxW, newQ, newSumLen)
        where
          newQ = S.dropWhileL (\(_, leaving) -> leaving < fst c) $ waitingQ S.|> c
          newSumLen = sumLen + fromIntegral (length newQ) - 1
          maxW = max maxWaiting (length newQ - 1)
      (a,b,c) = foldl' f (0, S.empty, 0::Integer) arrivals
   in (fromIntegral c / fromIntegral (length cs), a)

task3 xs cs =
  let
    rs = zip xs $ fmap task1 cs
    cmp (_, (x1,y1)) (_, (x2, y2)) = if y1 - x1 < y2 - x2 then True else False
    f c1 c2 =if cmp c1 c2 then c1 else c2
  in foldr1 f rs


-- debug function
simulate :: RandomGen g => [g] -> IO ()
simulate gs =
  let
    ncustomers = 1000 : fmap (*2) ncustomers
    customers = [Red, Yellow, Blue]
    cs = fmap (\(c, i) -> queue (gs !! i) c) $ zip customers [1..]
  in
  forM_ [0..] $
    \i->(do
            let n = ncustomers !! i
            putStrLn $ "i=" ++ show i ++ ", n=" ++ show n
            putStrLn $ "waiting times: " ++ (show $ zip customers $ fmap (task1 . take n . queue (gs !! 0)) customers)
            putStrLn $ "queue length : " ++ (show $ zip customers $ fmap (task2 . take n . queue (gs !! 0)) customers)
            putStrLn $ "closest avg and max waiting time: " ++ (show $ task3 customers $ fmap (take n) cs)
            putStrLn "------------------------------------------------------------------"
        )

main :: IO ()
main = do
  g <- newStdGen
  let gs = generators g
  let customers = [Red, Yellow, Blue]
  let cs = fmap (\(c, i) -> queue (gs !! i) c) $ zip customers [1..]
  -- let l = [CustomerEntry 1 10, CustomerEntry 5 18, CustomerEntry 2 1, CustomerEntry 100 10, CustomerEntry 4 2]
  let n = 1000000
  putStrLn $ "Task 1 - Yellow customer waiting times: (avg,max)=" ++ (show $ task1 $ take n $ queue g Yellow)
  putStrLn $ "Task 2 - Avg and max queue length of Red customers: (avg, max)=" ++ (show $ task2 $ take n $ queue g Red)
  putStrLn $ "Task 3 - Closest avg and max waiting time: " ++ (show $ task3 customers $ fmap (take n) cs)
  -- simulate gs
