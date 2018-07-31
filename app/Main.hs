module Main where

import Data.List
import System.Random


buildGrid :: Int -> [[Int]]
buildGrid size = [[ randomRIO (0, 100 :: Int) |x<-[1..size]] | x<-[1..size]]


main :: IO ()
main = do
  putStr . show =<< buildGrid 5
