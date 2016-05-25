

#### 1) dobraLista

    dobraLista []
    > []

    dobraLista [0,0,0,0]
    > [0,0,0,0]

    dobraLista [0,1,2,3]
    > [0,2,4,6]


#### 2) tamanho

    tamanho []
    > 0

    tamanho [0]
    > 1

    tamanho [0,1,2,3]
    > 4

#### 3) produtoLista

    produtoLista []
    > *** Exception: _

    produtoLista [0]
    > 0

    produtoLista [1]
    > 1

    produtoLista [0,1,2,3]
    > 0

    produtoLista [1,2,3]
    > 6

#### 4) andLista

    andLista []
    > *** Exception: _

    andLista [False]
    > False

    andLista [True]
    > True

    andLista [True,False]
    > False

    andLista [True,True]
    > True

    andLista [True,True,False]
    > False



#### 5) concatLista

    concatLista []
    > []

    concatLista [[1,2,3]]
    > [1,2,3]

    concatLista [[1,2],[3,4],[5,6]]
    > [1,2,3,4,5,6]



#### 6) inverteLista

    inverteLista []
    > []

    inverteLista [1]
    > [1]

    inverteLista [1,2,3,4]
    > [4,3,2,1]
