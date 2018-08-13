module Main where

import Data.List
import System.Random
import Mine


--randomList :: RandomGen g => Int -> g -> [Int]
--randomList 0 _ = []
--randomList n g = do
--  (if (fst (randomR (1::Int,100::Int) g)) < 36 then 1 else 0) : randomList (n-1) (snd (randomR (1::Int,100::Int) g)) 


--buildGrid :: RandomGen g => Int -> [g] -> [[Int]]
--buildGrid size seed = [randomList size g| g <-seed]


--randList :: RandomGen g => Int -> g -> [g]
--randList 0 _ = []
--randList n g = (fst(split g)) : randList (n-1) (snd(split g))



main :: IO ()
main = do
  seed   <- newStdGen
  --print $ buildGrid 10 (randList 10 seed)
  let aleaMinas = genMines seed 5 10
  print $ setMines 10 aleaMinas
  print $ genNumbers 10 aleaMinas
