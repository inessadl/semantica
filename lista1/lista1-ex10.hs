--10. 	Supondo que exista uma função vendas:
--
--		vendas :: Int -> Int
--
-- 		que devolve a venda semanal de uma loja (ex: vendas 0 devolve as vendas
--		na semana 0, vendas 1 devolve as vendas na semana 1, etc. Implemente
--		uma função chamada vendaTotal, que recebe um argumento n e calcula
--		todas as vendas da semana 0 até a semana n. Observe que essa função deve
--		ser recursiva. Exemplo de calculo: As vendas da semana 0 até a semana 2,
--		podem ser calculados usando a seguinte formula: 
--
--		vendas 0 + vendas 1 + vendas 2

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

vendaTotal :: Int -> Int 		-- (recursão sem caso base estoura a pilha)
vendaTotal 0 = vendas 0
vendaTotal n = (vendas n) + vendaTotal (n-1)


