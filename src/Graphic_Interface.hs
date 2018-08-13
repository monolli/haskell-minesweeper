module Graphic_Interface where

import Control.Monad
import Data.List (intercalate)
import Data.List.Split (endBy)
import Data.Matrix
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Functions

-- tamanho do bloco e borda interna
sizeTile = (50,5) :: (Float, Float)

-- Renderiza o campo completamente
renderCampo :: Int -> Grid -> IO Picture
renderCampo t m =
  let cs = toList m                           -- todas as casas
      ds = toList $ discoverAll m           -- todas as casas descobertas (em caso de derrota)
      ps = [(i,j) | i <- [1..t], j <- [1..t]] -- posições
      gO = gameContinue m                         -- verifica se o jogo continua
  in case gO of Keep -> return $ pictures $ zipWith (casa2bloco t gO) cs ps
                _        -> return $ pictures $ zipWith (casa2bloco t gO) ds ps

-- Converte indices das matrizes para pixels
ind2pix :: Float -> Float -> (Int, Int) -> (Float, Float)
ind2pix janela bloco (i,j) = (x0 + xp, y0 + yp)
  where x0 = -janela / 2
        y0 =  janela / 2
        xp =  (fromIntegral j) * bloco - (bloco / 2)
        yp = -(fromIntegral i) * bloco + (bloco / 2)

-- Converte uma casa em uma figura do Gloss (t: tamanho do Grid; g: gameContinue; (i,j): índice)
casa2bloco :: Int -> GameStatus -> Tile -> (Int,Int) -> Picture
casa2bloco t g (s,b,p) (i,j) =
  let -- converter de (i,j) para pixels
      pos = ind2pix ((fst sizeTile) * fromIntegral t) (fst sizeTile) (i,j)
      -- adaptação da função translate
      translate' (a,b) = translate a b
      -- adaptação da função traslate posicionar texto de potuação (empirico)
      translateT (a,b) = translate (a - 35 * e) (b - 50 * e)
      -- lado do quadrado que forma uma casa
      l = (fst sizeTile) - (snd sizeTile)
      -- reduzir escala para posicionar os textos de pontuação (empirico)
      e = ((fst sizeTile) - 3) / 200
  in case s of Flag    -> translate' pos $ color blue $ rectangleSolid l l
               Covered    -> translate' pos $ color (greyN 0.5) $ rectangleSolid l l
               Uncovered -> if p == 0 -- agir de acordo com a pontuação da casa
                              then translate' pos $ color (greyN 0.5) $ rectangleWire l l
                              else if b && (g == Defeat)
                                   then pictures [ translate' pos $ color red $ rectangleSolid l l, translateT pos $ scale e e $ color white $ text "X" ]
                                   else if b && (g == Victory)
                                        then pictures [ translate' pos $ color violet $ rectangleSolid l l, translateT pos $ scale e e $ color white $ text "O" ]
                                        else pictures [ translateT pos $ scale e e $ text (show p), translate' pos $ color (greyN 0.5) $ rectangleWire l l ]

--------------------------------------------------------------------------------------------

usoMain :: IO ()
usoMain = do
    putStrLn $ "Bem vindo ao MineSweeper"
    getLine >> return ()

-- Capturar clique (t: tamanho do campo)
capturaClique :: Int -> Event -> Grid -> IO Grid
capturaClique t (EventKey k s m (x,y)) c
   | gameContinue c == Defeat || gameContinue c == Victory
        = return c
   | k == MouseButton LeftButton && s == Down
        = return $ maybe c (discoverGrid c) $ pix2pos t (x,y)
   | k == MouseButton RightButton && s == Down
        = return $ maybe c (setTile c)    $ pix2pos t (x,y)
   | otherwise
        = return c
capturaClique _ _ c = return c

-- Converte pixels para indice das matrizes (c: largura da casa, b: borda da casa)
pix2pos :: Int -> (Float,Float) -> Maybe Pos
pix2pos t (x,y)
    | (abs x) > lmax || (abs y) > lmax                                = Nothing
    | (x' `mod` c') < b' `div` 2 || (x' `mod` c') > (b' `div` 2) + c' = Nothing
    | (y' `mod` c') < b' `div` 2 || (y' `mod` c') > (b' `div` 2) + c' = Nothing
    | otherwise = return $ (1 + y' `div` c' , 1 + x' `div` c')
        where (c,b)   = sizeTile
              lmax    = c * (fromIntegral t) / 2
              (c',b') = (round c, round b)
              (x',y') = (round $ lmax + x, round $ abs $ y - lmax) :: (Int, Int)
