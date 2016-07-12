-- -------------------------------------------------------- --
-- Semântica Formal (2016/01)
--
-- Nome: Inessa Luerce
-- Matrícula: 11106289
--
-- Trabalho 2: Completar a implementação da semântica Small-Step
-- no código fornecido e adicionar tratamento de exceções
--
-- -------------------------------------------------------- --


import Estado

data AExp = Num Int
    | Var String
	  | Som AExp AExp
    | Sub AExp AExp
	  | Mul AExp AExp
  deriving(Show)

data BExp =	TRUE
		| FALSE
    | Not BExp
		| And BExp BExp
    | Or  BExp BExp
		| Ig  AExp AExp
  deriving(Show)

data CExp = While BExp CExp
		| If BExp CExp CExp
		| Seq CExp CExp
		| Atrib AExp AExp
		| DoWhile BExp CExp
		| RepeatUntil CExp BExp
		| Throw
		| Try CExp CExp CExp
		| Catch
		| Skip
	deriving(Show)


-- ---------------- EXPRESSÕES ARITMÉTICAS -------------------- --

-- Verifica se continuará interpretando as expressões aritméticas
interpretA :: (AExp,Estado) -> (AExp,Estado)
interpretA (a,s) = if isFinalA a then (a,s) else interpretA (aSmallStep (a,s))

isFinalA :: AExp -> Bool
isFinalA (Num a) = True
isFinalA x = False

-- Interpreta expressões aritméticas
aSmallStep :: (AExp,Estado) -> (AExp,Estado)
aSmallStep (Var x,s) = (Num (procuraVar s x),s)
-- Soma
aSmallStep (Som (Num x) (Num y), s) = (Num (x+y),s)
aSmallStep (Som (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s) in (Som (Num x) ef,s)
aSmallStep (Som e1 e2,s)  = let (ef,_) = aSmallStep (e1, s) in (Som ef e2,s)
aSmallStep (Som exp1 exp2, s) = let (ef, _) = aSmallStep (exp1, s) in (Som ef exp2, s)
-- Subtração
aSmallStep (Sub (Num x) (Num y), s) = (Num (x-y), s)
aSmallStep (Sub (Num x) exp2, s) = let (ef, _) = aSmallStep (exp2, s) in (Sub (Num x) ef, s)
aSmallStep (Sub exp1 exp2, s) = let (ef, _) = aSmallStep (exp1, s) in (Sub ef exp2, s)
-- Multiplicação
aSmallStep (Mul (Num x) (Num y), s) = (Num (x*y), s)
aSmallStep (Mul (Num x) exp2, s) = let (ef, _) = aSmallStep (exp2, s) in (Mul (Num x) ef, s)
aSmallStep (Mul exp1 exp2, s) = let (ef, _) = aSmallStep (exp1, s) in (Mul ef exp2, s)


-- ---------------- EXPRESSÕES BOOLEANAS -------------------- --

-- Verifica se continuará interpretando as expressões booleanas
interpretB :: (BExp,Estado) -> (BExp,Estado)
interpretB (b,s) = if isFinalB b then (b,s) else interpretB (bSmallStep (b,s))

isFinalB :: BExp -> Bool
isFinalB TRUE = True
isFinalB FALSE = True
isFinalB x = False

-- Interpreta a expressão booleana.
bSmallStep :: (BExp, Estado) -> (BExp, Estado)
-- Not
bSmallStep (Not FALSE, s) = (TRUE, s)
bSmallStep (Not TRUE, s) = (FALSE, s)
bSmallStep (Not b, s) = let (bn, sn) = bSmallStep (b, s) in (Not bn, sn)
-- And
bSmallStep (And TRUE b2, s)	= (b2, s)
bSmallStep (And FALSE b2, s) = (FALSE,s)
bSmallStep (And b1 b2, s) = let (bn, sn) = bSmallStep (b1, s) in (And bn b2, sn)
-- Or
bSmallStep (Or FALSE b2, s) = (b2, s)
bSmallStep (Or TRUE b2, s) = (TRUE, s)
bSmallStep (Or b1 b2, s) = let (bn, sn) = bSmallStep (b1, s) in (Or bn b2, sn)
-- Equal
bSmallStep (Ig (Num x) (Num y), s) = if x==y then (TRUE, s) else (FALSE, s)
bSmallStep (Ig (Num x) e2, s) = let (ef, _) = aSmallStep (e2, s) in (Ig (Num x) ef, s)
bSmallStep (Ig e1 e2, s) = let (ef, _) = aSmallStep (e1 ,s) in (Ig ef e2, s)


-- ---------------- COMANDOS -------------------- --

-- Verifica se continua interpretando os comandos
interpretC :: (CExp, Estado) -> (CExp, Estado)
interpretC (c, s) = if isFinalC c then (c, s) else interpretC (cSmallStep (c, s))

-- Verifica se chegou no fim da interpretação do comando
isFinalC :: CExp -> Bool
isFinalC Skip = True
isFinalC x = False

-- Interpreta os comandos
cSmallStep :: (CExp,Estado) -> (CExp,Estado)
-- If
cSmallStep (If TRUE c1 c2, s) = (c1, s)
cSmallStep (If FALSE c1 c2, s) = (c2, s)
cSmallStep (If b c1 c2, s) = let (bn, sn) = bSmallStep (b, s) in (If bn c1 c2, sn)
-- Sequência
cSmallStep (Seq Skip c2,s) = (c2, s)
cSmallStep (Seq Throw c2, s) = (Throw, s)
cSmallStep (Seq c1 c2, s) = let (cn, sn) = cSmallStep (c1, s) in (Seq cn c2, sn)
-- Atribuição
cSmallStep (Atrib (Var x) (Num y), s) = (Skip, mudaVar (s) (x) (y))
cSmallStep (Atrib (Var x) e,s) = let (en,sn) = aSmallStep (e,s) in (Atrib (Var x) en,sn)
-- Repeat Until
cSmallStep (RepeatUntil Skip b, s) = (Skip, s)
cSmallStep (RepeatUntil c b, s) = let (cn, sn) = cSmallStep(c, s) in (Seq c (If b Skip (RepeatUntil cn b)), s)
-- While
cSmallStep (While b c, s) = (If b (Seq c (While b c)) Skip, s)
-- Do While
cSmallStep (DoWhile b c, s) = (Seq c (If b Skip (DoWhile b c)), s)
-- Try Skip Catch
cSmallStep (Try Skip Catch c, s) = (Skip, s) -- try skip não significa executar nada
-- Try Throw Catch
cSmallStep (Try Throw Catch c, s) = (c, s) -- lança exceção - executa catch
-- Try com comandos
cSmallStep (Try c1 Catch c2, s) = let (cn, sn) = cSmallStep(c1, s) in (Try cn Catch c2, sn)



-- ---------------- TESTES -------------------- --

-- ------- Exemplos de entradas e saídas: --------

-- Expressões Aritméticas

-- interpretA (exSoma1, meuEstado)
-- > (Num 315,[("x",3),("y",0),("z",0)])
--
-- interpretA (exSub1, meuEstado)
-- > (Num (-312),[("x",3),("y",0),("z",0)])
-- 
-- interpretA (exMul1, meuEstado)
-- > (Num 20,[("x",3),("y",0),("z",0)])

-- Soma
exSoma1 :: AExp
exSoma1 = Som (Num 313) (Num 2)
exSoma2 :: AExp
exSoma2 = Som (Num 10) (Som (Var "x") (Var "y"))
exSoma3 :: AExp
exSoma3 = Som (Som (Num 10) (Var "y")) (Som (Var "x") (Num 10))

-- Subtração
exSub1 :: AExp
exSub1 = Sub (Num 3) exSoma1
exSub2 :: AExp
exSub2 = Sub (Num 10) (Som (Var "x") (Num 5))
exSub3 :: AExp
exSub3 = Sub (Sub (Num 5) (Var "y")) (Sub (Var "x") (Num 10))

-- Multiplicação
exMul1 :: AExp
exMul1 = Mul (Num 4) (Num 5)
exMul2 :: AExp
exMul2 = Mul (Num 4) (Som (Var "x") (Num 5))
exMul3 :: AExp
exMul3 = Mul exSoma1 exSub1



-- Expressões Booleanas
--
-- interpretB (exBool1, meuEstado)
-- > (TRUE,[("x",3),("y",0),("z",0)])
--
-- interpretB (exBool2, meuEstado)
-- > (FALSE,[("x",3),("y",0),("z",0)])

-- And
exBool1 :: BExp
exBool1 = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE) 

-- Or
exBool2 :: BExp
exBool2 = Or (Not exBool1) (Or (Not TRUE) FALSE)



-- Comandos
--
-- interpretC (exC1, meuEstado)
-- > (Skip,[("x",313),("y",0),("z",0)])

-- interpretC (exC2, meuEstado)
-- > (Skip,[("x",3),("y",5),("z",0)])


-- Atribuição
exC1 :: CExp
exC1 = Atrib (Var "x") (Num 313)

-- If
exC2 :: CExp
exC2 = If FALSE exC1 (Atrib (Var "y") (Num 5))

-- Try (x := 222)
exC3 :: CExp
exC3 = Try (Seq Throw exC1) Catch (Atrib (Var "x")(Num 222))

-- Try (não entra no catch)
exC4 :: CExp
exC4 = Try (Seq Skip exC1) Catch (Atrib (Var "x")(Num 222))


-- Exemplos para teste fornecidos pelo professor:

meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]

exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))

-- RODANDO O EXEMPLO:
-- Hugs> interpretA (exemplo, meuEstado)

exemplo2 :: BExp
exemplo2 = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE)

-- *Main> interpretB (exemplo2,meuEstado)
-- (TRUE,[("x",3),("y",0),("z",0)])
