--------------------------------------------------------
----   Semântica Formal - Lista de Exercícios 1     ---- 
----   Inessa Diniz Luerce                          ----
--------------------------------------------------------


-- 1. Escreva a função osQuatroSaoIguais que possui tipo:
-- Int -> Int -> Int -> Int -> Bool

osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais a b c d = (a == b) && (b == c) && (c == d)


-- 2. Defina a função  quantosSaoIguais :: Int -> Int -> Int -> Int 
-- que conta quantos argumentos iguais a função recebeu.

quantosSaoIguais :: Int -> Int -> Int -> Int -> Int
quantosSaoIguais a b c d  | (a /= b) && (b /= c) && (c /= d) && (a /= c) && (a /= d) = 0       		  
                          | (a == b) && (b /= c) && (c /= d) && (a /= d) = 2
                          | (a == c) && (b /= c) && (c /= d) && (a /= b) = 2
                          | (a == d) && (a /= b) && (a /= c) && (c /= d) = 2
        		  | (b == c) && (a /= b) && (c /= d) && (a /= d) = 2
                          | (c == d) && (a /= b) && (b /= c) && (a /= d) = 2
                          | (a == b) && (b == c) && (c /= d) = 3
                          | (a /= b) && (b == c) && (c == d) = 3
        		  | (a == b) && (a == d) && (a /= c) = 3
        		  | (a == c) && (c == d) && (a /= b) = 3
        		  | otherwise = 4


-- 3. Defina a função  todosDiferentes :: Int -> Int -> Int -> Bool
-- que retorna True se todos os seus argumentos são diferentes.

todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c | (a /= b) && (a /= c) && (b /= c) = True
                      | otherwise = False


-- 4. Defina um conjunto de testes para a função todosDiferentes
--
-- > todosDiferentes n1 n2 n3 	10 20 30    True
-- > todosDiferentes n1 n2 n4 	10 20 20    False
-- > todosDiferentes n2 n4 n5 	20 20 20    False

n1 :: Int
n1 = 10

n2 :: Int
n2 = 20

n3 :: Int
n3 = 30

n4 :: Int
n4 = 20

n5 :: Int
n5 = 20


-- 5. O que está errado com a seguinte definição de todosDiferentes:
-- todosDiferentes n m p = ( ( n/=m ) && ( m/=p ) )
-- O conjunto de testes que você definiu na questão anterior funciona com
-- esta definição?

-- todosDiferentes n m p = ( ( n/=m ) && ( m/=p ) )

-- Casos de testes:
--
-- todosDiferentes n1 n2 n3 	10 20 30	True	ok
-- todosDiferentes n1 n2 n4 	10 20 20	False	ok
-- todosDiferentes n2 n4 n5 	20 20 20 	False	ok


-- 6. Defina a função:  todosIguais :: Int -> Int -> Int -> Bool

todosIguais :: Int -> Int -> Int -> Bool
todosIguais a b c | (a == b) && (b == c) = True
                  | otherwise = False


-- 7. Escreva uma definição de quantosSaoIguais que use a função 
-- todosDiferentes e a função todosIguais

quantosSaoIguais' :: Int -> Int -> Int -> Int
quantosSaoIguais' a b c
        | todosDiferentes a b c == True = 0
        | todosIguais a b c == True = 3
        | (a == b) && (b /= c) = 2
        | (a == c) && (b /= c) = 2
        | (b == c) && (b /= a) = 2


-- 8. Defina a função elevadoDois :: Int -> Int que recebe um argumento n e
-- devolve como resposta n^2

elevadoDois :: Int -> Int
elevadoDois n = n * n


-- 9. Defina a função elevadoQuatro :: Int -> Int que recebe um argumento n e
-- devolve como resposta n^4. Use elevadoDois para definir elevadoQuatro

elevadoQuatro :: Int -> Int
elevadoQuatro n = elevadoDois (elevadoDois n)


-- 10. Supondo que exista uma função vendas:
--
--     vendas :: Int -> Int

--     que devolve a venda semanal de uma loja (ex: vendas 0 devolve as vendas
--     na semana 0, vendas 1 devolve as vendas na semana 1, etc. Implemente
--     uma função chamada vendaTotal, que recebe um argumento n e calcula
--     todas as vendas da semana 0 até a semana n. Observe que essa função deve
--     ser recursiva. Exemplo de calculo: As vendas da semana 0 até a semana 2,
--     podem ser calculados usando a seguinte formula: 
--     vendas 0 + vendas 1 + vendas 2

vendas :: Int -> Int
vendas n
	| n == 0 = 12
	| n == 1 = 10
	| n == 2 = 11
	| otherwise = 22

-- Alternativa:
--
-- vendas :: Int -> Int
-- vendas 0 = 12
-- vendas 1 = 10
-- vendas 2 = 11
-- vendas _ = 22

vendaTotal :: Int -> Int
vendaTotal 0 = vendas 0
vendaTotal n = (vendas n) + vendaTotal (n-1)

