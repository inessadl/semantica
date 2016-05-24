--------------------------------------------------------
----   Semântica Formal - Lista de Exercícios 4     ---- 
----   Inessa Diniz Luerce                          ----
--------------------------------------------------------


-- 1. Defina a função membro :: [Int] -> Int -> Bool
-- que retorna um booleano que diz se o inteiro esta na lista

membro :: [Int] -> Int -> Bool
membro [] _ = False
membro (h:t) n | n == h = True
			   | otherwise = membro t n  -- t é a cauda ou o a lista "restante"   


-- 2. Implemente a função membroNum :: [Int] -> Int -> Int
-- que conta o número de vezes que o inteiro aparece na lista

membroNum :: [Int] -> Int -> Int
membroNum [] _ = 0
membroNum (h:t) n | n == h = 1 + membroNum t n 
				  | otherwise = membroNum t n


-- 3. Defina a função membro usando a função
-- membroNum l = lista n = numero

membro' :: [Int] -> Int -> Bool
-- membro' l n = if (membroNum l n == 0) then False else True
membro' l n  | membroNum l n == 0 = False
			 | otherwise = True


-- 4. Implemente a função  unico :: [Int] -> [Int] que retorna uma 
-- lista com os números que aparecem apenas uma vez na lista argumento. 
-- Ex:
-- Hugs> unico [2,4,1,4,1,3]
-- [2,3]
-- A função  memberNum deve ser usada na definição de unico

faux :: [Int] -> [Int] -> [Int]
faux _ [] = []
faux l (h:t) | membroNum l h == 1 = h: faux l t
			 | otherwise = faux l t

unico :: [Int] -> [Int]
unico l = faux l l


-- 5. Se a lista argumento para membro está ordenada, não é necessário 
-- percorrer toda a lista para saber se o elemento está presente na lista.
-- Implemente uma nova definição de membro, que use iSort

