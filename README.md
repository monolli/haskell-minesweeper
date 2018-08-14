# MineSweeper


## Funções e lógica do jogo


&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Antes de entrarmos efetivamente no uso da biblioteca Gloss, precisamos primeiro estabelecer as funções básicas do jogo, assim como sua dinâmica. Faremos toda essa lógica no arquivo functions.hs. Assim, criaremos as funções básicas que ditarão o funcionamento do jogo. A ideia desse bloco não é explicar linha a linha, mas sim mostrar o funcionamento de cada função e sua importância no contexto geral do jogo. Ao definirmos as funções básicas do Gloss na próxima etapa, procuraremos explicar com um maior nível de detalhamento.


### Functions.hs


&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;A tela principal do jogo é basicamente um tabuleiro (grid) de tamanho size x size (onde size será o número de linhas e o número de colunas). Assim, se escolhermos 10, a matriz será construída com 10 linhas e 10 colunas. Para a construção sessa matriz utilizaremos a biblioteca Data.Matrix.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Podemos imaginar que, inicialmente, precisamos gerar as posições aletórias das minas. Para isso, obteremos uma lista de posições aleatórias das minas que é gerada a cada nova rodada. Dessa forma, utilizaremos a função RandomRs e para obter nossa lista de posições aleatórias passaremos como argumento o tamanho da matriz (size x size), n que será a quantidade de minas que eu desejo no tabuleiro e seed (data StdGen) que será a semente da função Random.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Dessa forma chegamos a função genMines que produz as posições aleatórias das minas:

```sh
genMines :: StdGen -> Int -> Int -> [Pos]
genMines seed n size =  take n (nub . tup $ randomRs (1,size) seed)
                  where tup [] = []
                        tup (a:b:xs) = (a,b) : tup xs
```

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;A próxima necessidade é posicionar as minas no tabuleiro (matriz). Com a função setMines, é possível criar o tabuleiro de minas onde s é o tamanho do tabuleiro e ps são as posições das minas. Repare que no data.matrix, o primeiro size é o número de linhas, o segundo é o número de colunas e ``(\a -> a `elem` ps)`` é a função geradora.

```sh
setMines :: Int -> [Pos] -> Matrix Bool
setMines size ps = matrix size size (\a -> a `elem` ps)
```

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;A partir do momento que posicionamos as minas, uma parte fundamental do jogo é exibir os números ao redor das minas. Ou seja, um número aparece, indicando a quantidade de blocos adjacentes que contêm minas. A função genNumbers produz esses números no tabuleiro, tendo em vista o tamanho do tabuleiro s e as posições das minas ps.

```sh
genNumbers :: Int -> [Pos] -> Matrix Int
genNumbers size ps = matrix size size (gen ps)
  where gen ps (a,b) = length $ filter (flip elem ps) $ neighbors (a,b)
        neighbors (i,j) = [ (i-1 ,j-1) ,  (i ,j-1) , (i+1 ,j-1)
                          , (i-1 ,j)   ,  (i ,j  ) , (i+1 ,j  )
                          , (i-1 ,j+1) ,  (i ,j+1) , (i+1 ,j+1) ]
```

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Agora que escrevemos essas 3 funções, podemos escrever a função genGrid que gera o grid completo (e coberto) a partir das funções genMines, setMines e genNumbers

```sh
genGrid :: StdGen -> Int -> Int -> Grid
genGrid seed n size = elementwise2 (,,) (matrix size size (const Covered)) mines numbers
  where mines   = setMines size x
        numbers = genNumbers size x
        x       = genMines seed n size
        elementwise2 f matrixa matrixb matrixc = elementwise (\c (a,b) -> f a b c) matrixc
                                $ elementwise (\a b -> (a,b)) matrixa matrixb
```


### Graphic_Interface.hs


&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Uma vez implementadas as funções responsáveis pelo backend do jogo, pode-se começar a construir a parte gráfica. Entretanto, antes de começar a juntar as peças, é necessário definir algumas funções que serão úteis nos próximos passos.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Uma vez que será necessário exibir imagens em tela, é útil criar uma função para definir os pixels que servirão de base para imprimir as células do tabuleiro. Para isso, calcula-se o centro do tabuleiro, que é representado por "x0" e "y0", bem como a distância relativa ao centro, "xf" e "yf", onde a célula deve ser posicionada na imagem do tabueiro.

```sh
indexesToPixels :: Float -> Float -> (Int, Int) -> (Float, Float)
indexesToPixels window tile (i,j) = (x0 + xf, y0 + yf)
  where x0 = -window / 2
        y0 =  window / 2
        xf =  (fromIntegral j) * tile - (tile / 2)
        yf = -(fromIntegral i) * tile + (tile / 2)
```

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Em seguida, pode-se começar a definir os comportamentos que serão esperados do jogo. Para isto se faz necessário desenvolver uma função que, a partir das estruturas já criadas, estabelece os parâmetros que serão utilizados na exibição do jogo. Logo, dependendo da situação, a posição do tabuleiro deve ser exibida de uma maneira diferente, ou seja, o estado do jogo deve mudar.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Como a saída da função será uma "Picture", ou seja, objeto definido na biblioteca Gloss que permite criar impressões em tela, alguns ajustes determinados empiricamente precisaram ser realizados nos parâmetros de exibição, "transP" e "e", para que tudo fique alinhado.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Feito isto, pode-se inciar a definição das características gráficas de cada estado. Caso a posição em questão contenha uma "Flag", foi definido que a mesma será preenchida em azul, caso contrário, se a posição ainda estiver oculta, a mesma será preenchida em cinza, caso contrário, a posição está revelada e a quantidade de minas na vizinhança deve ser exibida. Ainda no úlltimo cenário, deve-se levar em consideração os casos em que revelar a posição acarreta em uma vitória ou em uma derrota.

```sh
tiletoGlossTile :: Int -> GameStatus -> Tile -> (Int,Int) -> Picture
tiletoGlossTile t g (s,b,p) (i,j) =
  let pos = indexesToPixels ((fst sizeTile) * fromIntegral t) (fst sizeTile) (i,j)
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
```

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Uma vez definido o fluxo de estados do jogo, é necessário renderizar os objetos "Picture" criados através da biblioteca Gloss para que os mesmo possam ser efetivamente exibidos em tela. Para isso, criou-se uma função que verifica se o jogo terminou, e com base nisso, gera ou a "Picture" do jogo para que o jogador possa tomar a próxima ação ou a "Picture" com o final do jogo.

```sh
rendGrid :: Int -> Grid -> IO Picture
rendGrid t m =
  let tiles = toList m                           -- todas as casas
      tsd = toList $ discoverAll m               -- tiles descobertas em caso de Defeat
      positions = [(i,j) | i <- [1..t], j <- [1..t]] -- posições no grid
      gameover = gameContinue m                  -- vai checar se o jogo continua
  in case gameover of Keep -> return $ pictures $ zipWith (tiletoGlossTile t gameover) tiles positions
                      _        -> return $ pictures $ zipWith (tiletoGlossTile t gameover) tsd positions
```


&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Apesar de os estados do jogo já estarem definidos, para que o mesmo seja funcional ainda faz-se necessário permitir que o usuário interaja com os mesmo. Para isso, será útil uma função que seja capaz de, dado um pixel, informar qual é a posição correspondente no tabuleiro àquele pixel.

```sh
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
```


&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Com a conversão de pixel para posição do tabuleiro pronta, faz-se possível o desenvolvimento de uma função capaz de identificar e interpretar a interação do usuário com a imagem que está sendo exibida em tela.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Caso o jogo tenha atingido um estado final, não é necessário interpretar a ação do usuário detro do tabueiro. Entretanto, se o usuário, em um estado não terminal, interagir com uma célula utilizando o botão esquerdo do mouse, a mesma deve ter o seu valor revelado. Caso a interação ocorra de maneira análoga à descrita acima, mas com o botão direito do mouse, a célula deve ter o seu estado alterado para representar uma "flag", que sinaliza a existência de uma bomba naquela posição. Em última instância, se o usuário interagir com o tabuleiro através de uma evento não previsto, o estado do jogo deve permanecer o mesmo.

```sh
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
```
