module Main where

import Data.List
import System.Random
import System.IO.Unsafe


randomMines :: Int
randomMines = unsafePerformIO $ getStdRandom (randomR (1,10))

randomList :: RandomGen g => Int -> g -> [Int]
randomList 0 _ = []
randomList n g = do
  (if (fst (randomR (1::Int,100::Int) g)) < 36 then 1 else 0) : randomList (n-1) (snd (randomR (1::Int,100::Int) g))


buildGrid :: RandomGen g => Int -> g -> [Int]
buildGrid size seed = randomList (size^2) seed




main :: IO ()
main = do
  seed <- newStdGen
  print $ buildGrid 3 seed
--  print $ randomList
