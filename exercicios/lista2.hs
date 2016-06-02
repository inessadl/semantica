--------------------------------------------------------
----   Semântica Formal - Lista de Exercícios 2     ----
----   Inessa Diniz Luerce                          ----
--------------------------------------------------------


-- 1. Defina uma função  max :: Int -> Int -> Int  que
-- retorna o maior entre dois valores
maximo :: Int -> Int -> Int
maximo a b
  | a <= b = b
  | otherwise = a

-- 2. Usando a função max, defina uma função maiorVenda que recebe um
-- argumento numérico n e calcula a maior venda em uma semana entre 0 e n.
vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 0
vendas 2 = 18
vendas _ = 20

maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0
maiorVenda n = maximo (vendas n)(maiorVenda (n-1))

-- 3. Defina uma função maxVenda que recebe um argumento numérico e calcula
-- a semana, entre 0 e n, que teve o maior n ́umero de vendas. Essa função
-- deve usar maiorVenda em sua definição
maxVenda :: Int -> Int
maxVenda 0 = 0                          -- caso base
maxVenda n
  | vendas n > maiorVenda (n-1) = n     -- se a semana atual é maior que as outras semanas retorna ela
  | otherwise = maxVenda (n-1)          -- caso contrário, busca recursivamente nas semanas anteriores

-- 4. Defina uma função zeroVendas que recebe um argumento numérico n e
-- que calcula qual das semanas entre 0 e n teve vendas igual a 0.
-- Se nenhuma semana teve vendas igual a 0 a função retorna -1
zeroVendas :: Int -> Int
zeroVendas 0
	| vendas 0 == 0 = 0
	| otherwise = -1
zeroVendas n
	| vendas n == 0 = n
	| otherwise = zeroVendas (n-1)


-- 5. Usando a definição anterior como guia, defina uma função que receba um
-- valor s e uma semana n, e devolva qual das semanas entre 0 e n teve vendas
-- iguais a s.
vendasN :: Int -> Int -> Int
vendasN s 0						-- se semanas é igual a 0
	| vendas 0 == s = 0 		-- e o valor das vendas for igual a s retorna semana 0
	| otherwise = -1
vendasN s n
	| vendas n == s = n
	| otherwise = vendasN (n-1)

-- Alternativa:
--
--vendasN' :: Int -> Int -> Int
--vendasN' _ (-1)	= -1			-- se nenhuma semana possui o valor
--vendasN' s n
--	| vendas n == s = n
--	| otherwise = vendasN' (n-1)

-- 6. Como você usaria a função anterior para definir a função zeroVendas
zeroVendas' :: Int -> Int
zeroVendas' n = vendasN 0 n

-- 7. As funções definidas até agora operam em um período entre 0 e n.
-- Defina versões alternativas dessas funções que trabalhem em um periodo
-- entre m e n, assumindo que n sempre  ́e maior que m.
maxVenda' :: Int -> Int -> Int
maxVenda' m n
	| m == n = vendas n             			-- caso base
	| vendas n > maxVenda' m (n-1) = vendas n 	-- se é maior retorna o valor de vendas
	| otherwise = maxVenda' m (n-1)				-- caso contrário retorna o maior valor (anterior)

-- 8. O fatorial de um número positivo n é
-- 1 * 2 * ... * (n-1) * n
-- Defina uma função fatorial em Haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

-- 9. Defina uma função que receba dois argumentos m e n e retorne o produto
-- m * (m+1) * ... * (n-1) * n
produto :: Int -> Int -> Int
produto m n
	| m == n = n
	| otherwise = m * produto (m+1) n


-- 10. Considere a sequência fibonacci de números: 0, 1, 1, 2, 3, 5, ...
-- cujos dois primeiros valores são 0 e 1, e os valores seguintes são sempre
-- a soma dos dois valores anteriores
-- (0+1=1, 1+1=2, 1+2=3, ...)
-- Escreva em Haskell a função fib sendo que fib n devolve o número que
-- esta na posição n da sequência fibonacci
fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo(n-2) + fibo(n-1)
