## Lista 1 - Casos de teste e resultados esperados


#### 1. osQuatroSaoIguais


    osQuatroSaoIguais 0 0 0 0
    > True

    osQuatroSaoIguais 0 1 0 0
    > False

    osQuatroSaoIguais 0 0 1 0
    > False

    osQuatroSaoIguais 0 0 0 1
    > False

    osQuatroSaoIguais 0 0 1 1
    > False

    osQuatroSaoIguais 0 0 0 1
    > False

    osQuatroSaoIguais 0 1 1 1
    > False


#### 2) quantosSaoIguais

    quantosSaoIguais 0 1 2 3
    > 0

    quantosSaoIguais 1 1 2 3
    > 2

    quantosSaoIguais 1 0 1 2
    > 2

    quantosSaoIguais 1 2 3 1
    > 2

    quantosSaoIguais 1 2 2 3
    > 2

    quantosSaoIguais 1 2 3 3
    > 2

    quantosSaoIguais 0 0 0 1
    > 3

    quantosSaoIguais 0 1 1 1
    > 3

    quantosSaoIguais 0 0 1 0
    > 3

    quantosSaoIguais 0 1 0 0
    > 3

    quantosSaoIguais 0 0 0 0
    > 4


#### 3) todosDiferentes

    todosDiferentes 0 1 2
    > True

    todosDiferentes 0 1 1
    > False

    todosDiferentes 1 0 1
    > False

    todosDiferentes 1 1 1
    > False



#### 6) todosIguais

    todosIguais 0 0 0
    > True

    todosIguais 0 1 1
    > False

    todosIguais 0 1 0
    > False

    todosIguais 0 1 2
    > False



#### 7) quantosSaoIguais'


    quantosSaoIguais' 0 1 2
    > 0

    quantosSaoIguais' 0 0 1
    > 2

    quantosSaoIguais' 0 1 0
    > 2

    quantosSaoIguais' 0 1 1
    > 2

    quantosSaoIguais' 0 0 0
    > 3



#### 8) elevadoDois

    elevadoDois 0
    > 0

    elevadoDois 1
    > 1

    elevadoDois 2
    > 4

    elevadoDois 4
    > 16



#### 9) elevadoQuatro

    elevadoQuatro 0
    > 0

    elevadoQuatro 1
    > 1

    elevadoQuatro 2
    > 16



### 10) vendas

    vendas 0
    > 12

    vendas 1
    > 10

    vendas 2
    > 11

    vendas 3
    > 22

    vendas 4
    > 22

    vendas 5
    > 22
