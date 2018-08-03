module Main where

import Data.List
import System.Random
import System.IO.Unsafe


randomMines :: Int
randomMines = unsafePerformIO $ getStdRandom (randomR (1,10))

buildGrid :: Int -> [[Int]]
buildGrid size =
  do
    [[if randomMines < 3 then 1 else 0 |x<-[1..size]] | x<-[1..size]]


main :: IO ()
main = do
  print $ buildGrid 5
