--------------------------------------------------------
----   Semântica Formal - Lista de Exercícios 3     ----
----   Inessa Diniz Luerce                          ----
--------------------------------------------------------


-- 1. Implemente a função  dobraLista :: [Int] -> [Int]
-- que dobra o valor de todos os elementos de uma lista

dobraLista :: [Int] -> [Int]
dobraLista [] = []
dobraLista (h:t) = h * 2 : dobraLista t


-- 2. Implemente a função  tamanho :: [Int] -> Int
-- que conta o número de elementos de uma lista

tamanho :: [Int] -> Int
tamanho [] = 0
tamanho(h:t) = 1 + tamanho t


-- 3. Implemente a função  produtoLista :: [Int] -> Int
-- que implementa o produto de uma lista de inteiros.

produtoLista :: [Int] -> Int
produtoLista [] = error "_"
produtoLista [n] = n
produtoLista (h:t) = h * produtoLista t


-- 4. Implemente a função  andLista :: [Bool] -> Bool
-- que faz um and (&&) entre todos os elementos de uma lista

andLista :: [Bool] -> Bool
andLista[] = error "_"
andLista[False] = False
andLista[True] = True
andLista (h:t) = h && andLista t


-- 5. Implemente a função  concatLista :: [[Int]] ->[Int]
-- que transforma uma lista de lista de inteiros em uma lista única de inteiros.
-- Ex:  Hugs> concatLista [[1,2,3], [2], [4,5]]
--		[1,2,3,2,4,5]

concatLista :: [[Int]] -> [Int]
concatLista[] = []
concatLista (h:t) = h ++ concatLista t


-- 6. Implemente a função  inverteLista :: [Int] -> [Int]
-- Ex.  Hugs> inverteLista [1,2,3,4]
--		[4,3,2,1]

inverteLista :: [Int] -> [Int]
inverteLista[] = []
inverteLista[h] = [h]
inverteLista(h:t) =  inverteLista t ++ [h]
