module Main where

import Data.List
import System.Random


randomMines :: IO Int
randomMines = getStdRandom (randomR (1,10))

buildGrid :: Int -> [[IO Int]]
buildGrid size =
  do
    m <- randomMines
    [[if m < 3 then 1 else 0 |x<-[1..size]] | x<-[1..size]]


main :: IO ()
main = do
  putStr . show =<< head(head (buildGrid 5))
