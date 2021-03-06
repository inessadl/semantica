-- -------------------------------------------------------- --
-- Semântica Formal (2016/01)
--
-- Nome: Inessa Luerce
-- Matrícula: 11106289
--
-- Trabalho 1: finalizar a implementação do interpretador da
-- Linguagem Imperativa (Big-Step) e adicionar os comandos:
--
-- a) do C while B
-- b) repeat C until B
-- c) loop E C
-- d) x, y := E1, E2
--
--
-- Exemplo de execução: > abigStep(exemplo,meuEstado)
-- -------------------------------------------------------- --

import Estado


data AExp = Num Int
      | Var String
      | Som AExp AExp
      | Sub AExp AExp
      | Mul AExp AExp
    deriving(Show)

data BExp = TRUE
      | FALSE
      | Not BExp
      | And BExp BExp
      | Or BExp BExp
      | Ig AExp AExp
      | Leq AExp AExp
    deriving(Show)

data CExp = While BExp CExp
      | If BExp CExp CExp
      | Seq CExp CExp
      | Atrib AExp AExp
      | DoWhile CExp BExp
      | RepeatUntil CExp BExp
      | Loop Int CExp
      | DuplaAtrib AExp AExp AExp AExp
      | Skip
    deriving(Show)


-- ------------ EXPRESSÕES ARITMÉTICAS --------------- --

abigStep :: (AExp,Estado) -> (Int,Estado)
abigStep (Var x,s) = (procuraVar s x,s)
abigStep (Num n,s) = (n,s)
abigStep (Som e1 e2,s) = let (n1,s1) = abigStep (e1, s);
							(n2,s2) = abigStep (e2, s)
							in (n1+n2,s)
abigStep (Sub e1 e2,s) = let (n1, s1) = abigStep (e1, s);
							(n2, s2) = abigStep (e2, s)
							in (n1-n2,s)
abigStep (Mul e1 e2,s) = let (n1, s1) = abigStep (e1, s);
							(n2, s2) = abigStep (e2, s)
							in (n1*n2, s)


-- ------------ EXPRESSÕES BOOLEANAS --------------- --

bbigStep :: (BExp,Estado) -> (Bool,Estado)
bbigStep (TRUE,s) = (True,s)
bbigStep (FALSE,s) = (False,s)
bbigStep (Not b,s) = let (b1,s1) = bbigStep (b,s)
							in (not b1,s1)
bbigStep (Ig e1 e2, s) = let (n1,s1) = abigStep(e1,s);
							(n2,s2) = abigStep (e2,s)
							in (n1 == n2, s)
bbigStep (And e1 e2, s) = let (b1, s1) = bbigStep(e1,s);
							(b2, s2) = bbigStep(e2,s)
							in (b1 && b2, s)
bbigStep (Or e1 e2, s) = let (b1, s1) = bbigStep(e1, s);
							(b2, s2) = bbigStep(e2, s)
							in (b1 || b2, s)
bbigStep (Leq e1 e2, s) = let (n1,s1) = abigStep(e1,s);
							(n2,s2) = abigStep(e2,s)
							in (n1 <= n2, s)


-- ------------ COMANDOS --------------- --

cbigStep :: (CExp,Estado) -> (CExp,Estado)
cbigStep (Skip,s) = (Skip,s)
cbigStep (While b c, s) = let (b1,s1) = bbigStep (b,s)
							in case b1 of
								True -> let (_,s2) = cbigStep(c,s);
										(_,s3) = cbigStep (While b c,s2)
										in (Skip,s3)
								False -> (Skip,s)
cbigStep (If b c1 c2,s) = let (b1, s1) = bbigStep(b, s)
							in case b1 of
								True -> let (_,s2) = cbigStep(c1, s1);
										in (Skip, s2)
								False -> let (_,s2) = cbigStep(c2, s2)
										in (Skip, s2)
cbigStep (Seq c1 c2,s)  = let (_, s1) = cbigStep (c1, s);
							(_, s2) = cbigStep (c2, s1)
							in (Skip, s2)
-- Atrib : retorna Skip com a memória modificada
cbigStep (Atrib (Var x) e,s) = let (e1,s1) = abigStep(e,s);
							in (Skip, mudaVar s x e1)
cbigStep(DoWhile c b, s) = let (_, s1) = cbigStep(c, s);
							(b1, s2) = bbigStep(b, s1)
							in case b1 of
								True -> let (e1, s3) = cbigStep(DoWhile c b, s1) in (e1, s3)
								False -> cbigStep(Skip, s1)
cbigStep(RepeatUntil c b, s) = let (_,s1) = cbigStep(c,s);
							(b1, s2) = bbigStep(b, s1)
							in case b1 of
								True -> cbigStep(Skip, s1)
								False -> let (e1, s3) = cbigStep(RepeatUntil c b, s1) in (e1, s3)
cbigStep(Loop e c, s) = if (e > 0) then let (_,s1) = cbigStep(c, s) in cbigStep(Loop (e-1) c, s1) else cbigStep(Skip, s)
cbigStep(DuplaAtrib x y e1 e2, s) = cbigStep(Seq (Atrib x e1)(Atrib y e2), s)



-- Os exemplos a seguir são apenas formas mais compactas de testar as 
-- operações, é possível executá-las explicitamente no terminal
--
-- Exemplos de execução:
--
-- cbigStep(fatorial, meuEstado)
-- > (Skip,[("x",1),("y",6),("z",0)])
--
-- cbigStep(fatorial, estadoXY)
-- > (Skip,[("x",1),("y",3628800)])
--
-- cbigStep(DuplaAtrib(Var "x")(Var "y") (Num 313) (Num 144), est1)
-- > (Skip,[("x",313),("y",144)])
--
-- cbigStep(exLoop, [("x", 0)])
-- > (Skip,[("x",10)])
--
-- cbigStep(exLoop, estadoXY)
-- > (Skip,[("x",20),("y",15)])


-- Estado com as variáveis "x" e "y"
estadoXY :: Estado
estadoXY = [("x", 10), ("y", 15)]


-- Estados apenas com "x"
estadoX :: Estado
estadoX = [("x", 4)]

estadoX' :: Estado
estadoX' = [("x", 10)]


-- Soma
exSom :: AExp
exSom = Som (Num 5) (Som (Var "x") (Var "y"))

-- Subtração
exSub :: AExp
exSub = Sub (Var "x") (Var "y")

-- Multiplicação
exMul :: AExp
exMul = Mul (Var "x") (Var "y")

-- And
exAnd :: BExp
exAnd = And TRUE TRUE

-- Atribuição
exAtrib :: CExp
exAtrib = Atrib (Var "x") (Num 15)

-- Incrementa
exIncrementa :: CExp
exIncrementa = Atrib (Var "x") (Som (Var "x")(Num 1))

-- Incrementa2
exIncrementa2 :: CExp
exIncrementa2 = Atrib (Var "x") (Som (Var "x")(Num 2))

-- Less or equal
exLeq :: BExp
exLeq = Leq (Var "x")(Num 10)

-- Loop e c
exLoop :: CExp
exLoop = Loop 5 exIncrementa2

-- DoWhile
exDoWhile :: CExp
exDoWhile = DoWhile exIncrementa (Leq (Var "x")(Num 6))

-- Exemplos para teste dados pelo professor:

meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]

exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))


teste1 :: BExp
teste1 = (Ig (Som (Num 3) (Num 3))  (Mul (Num 2) (Num 3)))
teste2 :: BExp
teste2 = (Ig (Som (Var "x") (Num 3))  (Mul (Num 2) (Num 3)))


testec1 :: CExp
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y")))
		(Atrib (Var "y") (Var "z")))

fatorial :: CExp
fatorial = (Seq (Atrib (Var "y") (Num 1))
               (While (Not (Ig (Var "x") (Num 1)))
                      (Seq (Atrib (Var "y") (Mul (Var "y") (Var "x")))
                           (Atrib (Var "x") (Sub (Var "x") (Num 1))))))
