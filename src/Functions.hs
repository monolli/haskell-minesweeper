-- MineSweeper 1.0 by
-- Paradigmas de Programação

module Functions where

--Bibliotecas necessárias - Se não usar o Stack, será necessário instalar 'cabal install matrix' e 'cabal install random'
import Data.Matrix
import Data.List
import System.Random

-- estado do campo minado - campo = Grid
type Grid = Matrix Tile

-- informações sobre a casa (Tile)
type Tile = ( State, Bool, Int )
-- se está marcada ou não -- se há bomba ou não -- a pontuação da casa

-- estado de um casa do campo
data State = Flag | Covered | Uncovered
           deriving (Eq)
-- casa foi sinalizada com possível bomba  -- casa ainda coberta   -- casa descoberta

-- estado geral do jogo
data GameStatus = Keep | Defeat | Victory
              deriving (Eq,Show)
-- jogo continua -- sinaliza derrota no jogo -- sinaliza vitória no jogo

type Pos = (Int, Int) -- type de dois inteiros para posição

{-A função genGrid gera o grid completo (e coberto) a partir das funções genMines, setMines e genNumbers-}
genGrid :: StdGen -> Int -> Int -> Grid
genGrid seed n size = elementwise2 (,,) (matrix size size (const Covered)) mines numbers
  where mines   = setMines size x
        numbers = genNumbers size x
        x       = genMines seed n size
        elementwise2 f matrixa matrixb matrixc = elementwise (\c (a,b) -> f a b c) matrixc
                                $ elementwise (\a b -> (a,b)) matrixa matrixb

{-A função genMines produz as posições aleatórias das minas, onde n é a quantidade de minas, t e o tamanho da lista e g é a seed para o random-}
genMines :: StdGen -> Int -> Int -> [Pos]
genMines seed n size =  take n (nub . tup $ randomRs (1,size) seed) -- nub retira os elementos repetidos
                  where tup [] = []
                        tup (a:b:xs) = (a,b) : tup xs

{-A função setMines cria a malha de minas onde s é o tamanho da malha e ps são as posições das minas-}
setMines :: Int -> [Pos] -> Matrix Bool
setMines size ps = matrix size size (\a -> a `elem` ps)
-- No data.matrix, o primeiro s é o número de linha, o segundo é o número de colunas e (\a -> a `elem` ps) é a função geradora.

{-A função genNumbers produz os números ou pontos da malha, tendo em vista o tamanho da malha s e as posições das minas ps -}
genNumbers :: Int -> [Pos] -> Matrix Int
genNumbers size ps = matrix size size (gen ps)
  where gen ps (a,b) = length $ filter (flip elem ps) $ neighbors (a,b)
        neighbors (i,j) = [ (i-1 ,j-1) ,  (i ,j-1) , (i+1 ,j-1)
                          , (i-1 ,j)   ,  (i ,j  ) , (i+1 ,j  )
                          , (i-1 ,j+1) ,  (i ,j+1) , (i+1 ,j+1) ]

-- Descobrir o mapa após uma jogada
discoverGrid :: Grid -> Pos -> Grid
discoverGrid c (i,j) = seedFill (i,j) evaluate discover stop c
  where evaluate   (s,b,p) | s == Uncovered = True
                          | p > 0           = True
                          | otherwise       = False
        discover (s,b,p) | s == Covered    = (Uncovered,b,p)
                          | s == Flag    = (Uncovered,b,p)
                          | otherwise       = (s,b,p)
        stop             = discover

-- Flood fill em 4 direções para matrizes -- algoritmo de inundação
seedFill :: (Int,Int) -> (a -> Bool) -> (a -> a) -> (a -> a) -> Matrix a -> Matrix a
-- Posicao incial -- Funçao de condiçao -- Funçao de mudança (se condição foi falsa) -- Função de parada (se condição foi verdadeira)
-- Matriz de entrada  -- Matriz de saída
seedFill (i,j) conditional change stp matrix_in
    |  i < 1 || i > nrows matrix_in
    || j < 1 || j > ncols matrix_in = matrix_in
    | conditional x == True  =      setElem (stp x) (i,j) matrix_in
    | conditional x == False = f' $ setElem (change x) (i,j) matrix_in
      where x    = getElem i j matrix_in
            f' m = seedFill (i+1,j) conditional change stp $
                   seedFill (i-1,j) conditional change stp $
                   seedFill (i,j+1) conditional change stp $
                   seedFill (i,j-1) conditional change stp m

-- Marcar casa
setTile :: Grid -> Pos ->  Grid
setTile m (i,j) = let (s,b,p) = getElem i j m in
                     case s of Flag    -> setElem (Covered,b,p) (i,j) m
                               Covered    -> setElem (Flag,b,p) (i,j) m
                               Uncovered -> m

-- Descobrir todo o mapa em caso de derrota
discoverAll :: Grid -> Grid
discoverAll grid = matrix (nrows grid) (ncols grid) func
  where func (i,j) = (\(_,m,n) -> (Uncovered,m,n)) (getElem i j grid)

-- Ação no mapa
--actionGrid :: Grid -> (Pos, State) -> Grid
--actionGrid m ((i,j),Flag) = setTile    m (i,j)
--actionGrid m ((i,j),_)       = discoverGrid m (i,j)

-- Verifica se o jogo continua ou é interrompido
gameContinue :: Grid -> GameStatus
gameContinue jogo = worker True $ toList jogo
  where worker True   [] = Victory
        worker False  [] = Keep
        worker m ((s,b,p):xs)
            |  s == Uncovered               && b     = Defeat
            | (s == Covered || s == Flag) && not b = worker False xs
            | otherwise = worker m xs
