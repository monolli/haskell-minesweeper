module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Environment
import System.Random

import Functions
import Graphic_Interface

--playGMiner recebe a semente para o Ramdon , a qtd de minas n e o tamanho do grid t
playGMiner :: StdGen -> Int -> Int -> IO()
playGMiner g n t = let s = t * (fromEnum $ fst sizeTile)
               in playIO (InWindow  "MineSweeper | Paradigmas de Programacao" (s, s) (640,240))
                          -- argumentos necessários para o IO.Game
                         (greyN 1.00)        -- background
                         30                  -- qtd de frames por segundo
                         (genGrid g n t)
                         (rendGrid t)
                         (getClick t)
                        (\_ c -> return c)  -- tempo

main :: IO ()
main = do a <- getArgs
          g <- getStdGen
          if length a /= 2
          then graphicMain
          else let n = read (a !! 0) :: Int
                   t = read (a !! 1) :: Int
                in playGMiner g n t
