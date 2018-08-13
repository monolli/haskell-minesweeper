# MineSweeper


...

## Funções e lógica do jogo

Antes de entrarmos efetivamente no uso da biblioteca Gloss, precisamos primeiro estabelecer as funções básicas do jogo, assim como sua dinâmica. Faremos toda essa lógica no arquivo functions.hs. Assim, criaremos as funções básicas que ditarão o funcionamento do jogo. A ideia desse bloco não é explicar linha a linha, mas sim mostrar o funcionamento de cada função e sua importância no contexto geral do jogo. Ao definirmos as funções básicas do Gloss na próxima etapa, procuraremos explicar com um maior nível de detalhamento.

### Functions.hs


A tela principal do jogo é basicamente um tabuleiro (grid) de tamanho size x size (onde size será o número de linhas e o número de colunas). Assim, se escolhermos 10, a matriz será construída com 10 linhas e 10 colunas. Para a construção sessa matriz utilizaremos a biblioteca Data.Matrix.

Podemos imaginar que, inicialmente, precisamos gerar as posições aletórias das minas. Para isso, obteremos uma lista de posições aleatórias das minas que é gerada a cada nova rodada. Dessa forma, utilizaremos a função RandomRs e para obter nossa lista de posições aleatórias passaremos como argumento o tamanho da matriz (size x size), n que será a quantidade de minas que eu desejo no tabuleiro e seed (data StdGen) que será a semente da função Random.

Dessa forma chegamos a função genMines que produz as posições aleatórias das minas:

```sh
genMines :: StdGen -> Int -> Int -> [Pos]
genMines seed n size =  take n (nub . tup $ randomRs (1,size) seed) -- nub retira os elementos repetidos
                  where tup [] = []
                        tup (a:b:xs) = (a,b) : tup xs
```

A próxima necessidade é posicionar as minas no tabuleiro (matriz). Com a função setMines, é possível criar o tabuleiro de minas onde s é o tamanho do tabuleiro e ps são as posições das minas. Repare que no data.matrix, o primeiro size é o número de linhas, o segundo é o número de colunas e ``(\a -> a `elem` ps)`` é a função geradora.

```sh
setMines :: Int -> [Pos] -> Matrix Bool
setMines size ps = matrix size size (\a -> a `elem` ps)
```

A partir do momento que posicionamos as minas, uma parte fundamental do jogo é exibir os números ao redor das minas. Ou seja, um número aparece, indicando a quantidade de blocos adjacentes que contêm minas. A função genNumbers produz esses números no tabuleiro, tendo em vista o tamanho do tabuleiro s e as posições das minas ps.

```sh
genNumbers :: Int -> [Pos] -> Matrix Int
genNumbers size ps = matrix size size (gen ps)
  where gen ps (a,b) = length $ filter (flip elem ps) $ neighbors (a,b)
        neighbors (i,j) = [ (i-1 ,j-1) ,  (i ,j-1) , (i+1 ,j-1)
                          , (i-1 ,j)   ,  (i ,j  ) , (i+1 ,j  )
                          , (i-1 ,j+1) ,  (i ,j+1) , (i+1 ,j+1) ]
```

Agora que escrevemos essas 3 funções, podemos escrever a função genGrid que gera o grid completo (e coberto) a partir das funções genMines, setMines e genNumbers

```sh
genGrid :: StdGen -> Int -> Int -> Grid
genGrid seed n size = elementwise2 (,,) (matrix size size (const Covered)) mines numbers
  where mines   = setMines size x
        numbers = genNumbers size x
        x       = genMines seed n size
        elementwise2 f matrixa matrixb matrixc = elementwise (\c (a,b) -> f a b c) matrixc
                                $ elementwise (\a b -> (a,b)) matrixa matrixb
```
