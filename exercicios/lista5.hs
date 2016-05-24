
data Arvore = Folha Int | No Int Arvore Arvore 
	deriving (Eq, Show)

arv1 :: Arvore
arv1 = No 2 (Folha 1)(Folha 3)

arv2 :: Arvore
arv2 = No 2 (No 2 (Folha 1)(Folha 3))(No 2 (Folha 1)(Folha 3))

arvDoRodrigo :: Arvore
arvDoRodrigo = No 6 (Folha 42)(No 2 (Folha 313)(Folha 54))

--1 Question ́ario

--1. Defina uma fun ̧c ̃ao que multiplique por 2 os inteiros em uma  ́arvore.
dobraArvore :: Arvore -> Arvore
dobraArvore (Folha n) = Folha (n*2)
dobraArvore (No n a1 a2) = No (n*2) (dobraArvore a1)(dobraArvore a2)


--2. Defina uma fun ̧c ̃ao que ache o maior elemento de uma  ́arvore
maior :: Int -> Int -> Int
maior a b
	| a <= b = b
	| otherwise = a 

maiorElem :: Arvore -> Int
maiorElem (Folha n) = n
maiorElem (No n a1 a2) = maior n (maior(maiorElem a1) (maiorElem a2))


--3. Defina a fun ̧c ̃ao que diz se um inteiro ocorre dentro de uma  ́arvore
ocorreNaArv :: Arvore -> Int -> Bool 
ocorreNaArv (Folha n) x | x == n = True
						| otherwise = False
ocorreNaArv (No n a1 a2) x = x == n || ocorreNaArv a1 x || ocorreNaArv a2 x 

--4. Defina uma fun ̧c ̃ao que ache o mair inteiro dentro de uma  ́arvore

--5. Defina uma fun ̧c ̃ao que diz quantas vezes um inteiro ocorre dentro de uma
-- ́arvore
quantasVezes :: Arvore -> Int -> Int
quantasVezes (Folha n) x | x == n = 1
						 | otherwise = 0
quantasVezes (No n a1 a2) x | x == n = 1 + quantasVezes a1 x + quantasVezes a2 x
						    | otherwise = quantasVezes a1 x + quantasVezes a2 x 

--6. Uma  ́arvore refletida  ́e uma  ́arvore com seus ramos esquerdos e direitos
--trocados. Defina uma fun ̧c ̃ao refleteArvore
reflete :: Arvore -> Arvore
reflete (Folha n) = (Folha n)
reflete (No n a1 a2) = (No n (reflete a2) (reflete a1))

--7. Defina uma fun ̧c ̃ao que transfore uma  ́arvore em uma lista
arvToList :: Arvore -> [Int]
arvToList (Folha n) = [n]
arvToList (No n a1 a2) = n: arvToList a1 ++ arvToList a2 

--8. Defina a fun ̧c ̃ao mapTree que aplica uma fun ̧c ̃ao a todos os inteiros de todos
--os n ́os de uma  ́arvore.
--mapTree :: (Arvore -> Arvore) -> Arvore -> Arvore

mapTree :: (Int -> Int) -> Arvore -> Arvore
mapTree f (Folha n) = Folha (f n)
mapTree f (No n a1 a2) = (No (f n) (mapTree f a1) (mapTree f a2))

--9. Defina uma lista como sendo um tipo alg ́ebrico recursivo. Defina as fun ̧c ̃oes
--tamanho (que conta o n ́umero de elementos de uma lista) e a fun ̧c ̃ao map
--que operem no tipo lista definido.
data List = Nodo Int Int | NULL 
	deriving (Eq, Show)

--tamanho :: List -> Int
--tamanho NULL = 0
--tamanho 

--map :: (Int -> Int) -> List -> List