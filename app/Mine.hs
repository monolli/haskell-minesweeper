module Mine where


import Data.Matrix
import Data.List
import System.Random

type Pos = (Int, Int) -- tupla de inteiros para posição

{-A função genMines produz as posições aleatórias das minas, onde n é a quantidade de minas, size e o tamanho da lista e seed é a semente para o random-}
genMines :: StdGen -> Int -> Int -> [Pos]
genMines seed n size =  take n (nub . tup $ randomRs (1,size) seed) -- nub retira os elementos repetidos
                  where tup [] = []
                        tup (a:b:xs) = (a,b) : tup xs

{-A função setMines cria a malha de minas onde s é o tamanho da malha e ps são as posições das minas-}
setMines :: Int -> [Pos] -> Matrix Bool
setMines s ps = matrix s s (\a -> a `elem` ps)

-- No data.matrix, o primeiro s é o número de linha, o segundo é o número de colunas e (\a -> a `elem` ps) é a função geradora. As demais funções seguem a mesma lógica


{-A função genNumbers produz os números ou pontos da malha, tendo em vista o tamanho da malha s e as posições das minas ps -}
genNumbers :: Int -> [Pos] -> Matrix Int
genNumbers s ps = matrix s s (gen ps)
                  where gen ps (a,b) = length $ filter (flip elem ps) $ neighbors (a,b)
                        neighbors (i,j) = [ (i-1,j-1) ,  (i,j-1) , (i+1,j-1)
                                          , (i-1,j) ,   (i,j  ) , (i+1,j  )
                                          , (i-1,j+1) , (i,j+1) , (i+1,j+1) ]
