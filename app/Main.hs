module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Environment
import System.Random

import Functions
import Graphic_Interface

jogarG :: StdGen -- semente para RNG
       -> Int    -- quantidade de bombas
       -> Int    -- tamanho do campo
       -> IO()   -- saída
jogarG g n t = let s = t * (fromEnum $ fst sizeTile)
               in playIO (InWindow  "MineSweeper | Paradigmas de Programacao" (s, s) (640,240))
                         (greyN 1.00)        -- cor de fundo
                         30                  -- fps
                         (genGrid g n t)  -- mapa inicial
                         (renderCampo t)     -- função para renderizar
                         (capturaClique t)   -- eventos de mouse
                         (\_ c -> return c)  -- passo de tempo

main :: IO ()
main = do a <- getArgs
          g <- getStdGen
          if length a /= 2
          then usoMain
          else let n = read (a !! 0) :: Int
                   t = read (a !! 1) :: Int
                in jogarG g n t
