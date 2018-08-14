module Graphic_Interface where

import Control.Monad
import Data.List (intercalate)
import Data.List.Split (endBy)
import Data.Matrix
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Functions


-- rendGrid vai retornar um IO Picture para renderizar o Grid todo
rendGrid :: Int -> Grid -> IO Picture
rendGrid t m =
  let tiles = toList m                           -- todas as casas
      tsd = toList $ discoverAll m               -- tiles descobertas em caso de Defeat
      positions = [(i,j) | i <- [1..t], j <- [1..t]] -- posições no grid
      gameover = gameContinue m                  -- vai checar se o jogo continua
  in case gameover of Keep -> return $ pictures $ zipWith (tiletoGlossTile t gameover) tiles positions
                      _        -> return $ pictures $ zipWith (tiletoGlossTile t gameover) tsd positions

-- A função indexesToPixels converte os indices i,j das matrizes para pixels
indexesToPixels :: Float -> Float -> (Int, Int) -> (Float, Float)
indexesToPixels window tile (i,j) = (x0 + xf, y0 + yf)
  where x0 = -window / 2
        y0 =  window / 2
        xf =  (fromIntegral j) * tile - (tile / 2)
        yf = -(fromIntegral i) * tile + (tile / 2)


sizeTile = (50,5) :: (Float, Float) -- tamanho do Tile e seu vértice (borda) interno

-- Converte uma casa em uma figura do Gloss (t: tamanho do Grid; g: gameContinue; (i,j): índice)
tiletoGlossTile :: Int -> GameStatus -> Tile -> (Int,Int) -> Picture
tiletoGlossTile t g (s,b,p) (i,j) =
  let pos = indexesToPixels ((fst sizeTile) * fromIntegral t) (fst sizeTile) (i,j) -- converter de (i,j) para pixels
      trans (a,b) = translate a b -- adaptação da função translate
      transP (a,b) = translate (a - 35 * e) (b - 50 * e) -- ajuste da função traslate para posicionar texto de potuação (empirico)
      l = (fst sizeTile) - (snd sizeTile) -- lado do quadrado que forma uma casa
      e = ((fst sizeTile) - 3) / 200 -- reduzir escala para posicionar os textos de pontuação (empirico)
  in case s of Flag    -> trans pos $ color blue $ rectangleSolid l l
               Covered    -> trans pos $ color (greyN 0.5) $ rectangleSolid l l
               Uncovered -> if p == 0 -- agir de acordo com a pontuação da casa
                              then trans pos $ color (greyN 0.5) $ rectangleWire l l
                              else if b && (g == Defeat)
                                   then pictures [ trans pos $ color red $ rectangleSolid l l, transP pos $ scale e e $ color white $ text "X" ]
                                   else if b && (g == Victory)
                                        then pictures [ trans pos $ color violet $ rectangleSolid l l, transP pos $ scale e e $ color white $ text ":D" ]
                                        else pictures [ transP pos $ scale e e $ text (show p), trans pos $ color (greyN 0.5) $ rectangleWire l l ]

graphicMain :: IO ()
graphicMain = do
    putStrLn $ "Bem vindo ao MineSweeper. Inicie com a qtd de minas e tamanho."
    getLine >> return ()

-- Pega os clicks do mouse (t: tamanho do grid)
getClick :: Int -> Event -> Grid -> IO Grid
getClick t (EventKey k s m (x,y)) c
   | gameContinue c == Defeat || gameContinue c == Victory
        = return c
   | k == MouseButton LeftButton && s == Down
        = return $ maybe c (discoverGrid c) $ pixelsToIndexes t (x,y)
   | k == MouseButton RightButton && s == Down
        = return $ maybe c (setTile c)    $ pixelsToIndexes t (x,y)
   | otherwise
        = return c
getClick _ _ c = return c

-- pixelsToIndexes converte pixels para indice das matrizes (c: largura da casa, b: borda da casa)
pixelsToIndexes :: Int -> (Float,Float) -> Maybe Pos
pixelsToIndexes t (x,y)
    | (abs x) > lmax || (abs y) > lmax                                = Nothing
    | (x' `mod` c') < b' `div` 2 || (x' `mod` c') > (b' `div` 2) + c' = Nothing
    | (y' `mod` c') < b' `div` 2 || (y' `mod` c') > (b' `div` 2) + c' = Nothing
    | otherwise = return $ (1 + y' `div` c' , 1 + x' `div` c')
        where (c,b)   = sizeTile
              lmax    = c * (fromIntegral t) / 2
              (c',b') = (round c, round b)
              (x',y') = (round $ lmax + x, round $ abs $ y - lmax) :: (Int, Int)
