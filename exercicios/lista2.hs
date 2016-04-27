
-- retorna o maior entre dois valores
maximo :: Int -> Int -> Int
maximo a b
  | a <= b = b
  | otherwise = a

-- retorna o valor associado às vendas das semanas
vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 0
vendas 2 = 18
vendas _ = 20

-- busca recursivamente durante as semanas e retorna o valor da maior venda
maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0
maiorVenda n = maximo (vendas n)(maiorVenda (n-1))

maxVenda :: Int -> Int
maxVenda 0 = 0                          -- caso base
maxVenda n
  | vendas n > maiorVenda (n-1) = n     -- se a semana atual é maior que as outras semanas retorna ela
  | otherwise = maxVenda (n-1)          -- caso contrário, busca recursivamente nas semanas anteriores


-- retorna a primeira semana encontrada que teve o número de vendas = 0
zeroVendas :: Int -> Int
zeroVendas 0
	| vendas 0 == 0 = 0
	| otherwise = -1
zeroVendas n
	| vendas n == 0 = n
	| otherwise = zeroVendas (n-1)

-- recebe um valor 's' e uma semana 'n', verifica se há alguma semana
-- com valor s
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

zeroVendas' :: Int -> Int
zeroVendas' n = vendasN 0 n

-- versão alternativa onde a recursão é entre m e n
maxVenda' :: Int -> Int -> Int
maxVenda' m n
	| m == n = vendas n             			-- caso base
	| vendas n > maxVenda' m (n-1) = vendas n 	-- se é maior retorna o valor de vendas
	| otherwise = maxVenda' m (n-1)				-- caso contrário retorna o maior valor (anterior)


factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)


produto :: Int -> Int -> Int
produto m n
	| m == n = n
	| otherwise = m * produto (m+1) n


fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo(n-2) + fibo(n-1)
