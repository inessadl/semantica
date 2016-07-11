
-- Semântica Formal (2016/01)
--
-- Trabalho 2:


data Exp = Num Int | TRUE | FALSE | Var String | Soma Exp Exp
	| Mult Exp Exp | And Exp Exp | Or Exp Exp | Not Exp | IF Exp Exp Exp
	| Ap Exp Exp | Fun String Tipo Exp | Let String Tipo Exp Exp
	deriving (Eq,Show)
data Tipo = INT | BOOL | F Tipo Tipo | THROW
	deriving (Eq, Show)

-- roda functional program
runFP :: Exp -> Exp
runFP exp = if isFinal exp then exp else runFP (smallStep exp)

-- para rodar runFP para garantir o tipo correto de retorno
isFinal :: Exp -> Bool
isFinal (Num n) = True
isFinal TRUE = True
isFinal FALSE = True
isFinal THROW = True
isFinal (Fun s t e) = True
isFinal _ = False


smallStep :: Exp -> Exp
smallStep (Soma (Num n1) (Num n2)) = Num (n1 + n2)
smallStep (Soma (Num n1) e2 ) = Soma (Num n1) (smallStep e2)
smallStep (Soma e1 e2) = Soma (smallStep e1) e2
-- smallStep

-- substitui variavel e o valor dela na expressao
subs :: String -> Exp -> Exp -> Exp
subs var val (Num v) = (Num v)
subs var val TRUE = TRUE
subs var val FALSE = FALSE
subs var val (Soma exp1 exp2) = Soma (subs var val exp1) (subs var val exp2)
subs var val (Mult exp1 exp2) = Mult (subs var val exp1) (subs var val exp2)
subs var val (And exp1 exp2) = And (subs var val exp1) (subs var val exp2)
subs var val (Or exp1 exp2) = Or (subs var val exp1) (subs var val exp2)
subs var val (Not exp1) = Not (subs var val exp1)
subs var val (IF b exp1 exp2) = IF (subs var val b) (subs var val exp1) (subs var val exp2)
subs var val (Let x t exp1 exp2) = Let x t (subs var val exp1) (subs var val exp2)
subs var val (Ap exp1 exp2) = Ap (subs var val exp1) (subs var val exp2)
subs var val (Fun x t exp1) = Fun x t (subs var val exp1)
subs var val (Var var1) | var == var1 = val
												| otherwise = (Var var1)

-- subs var val ? = ?
-- {v/x}e1 = subs x v e1

progTeste :: Exp
progTeste = Soma (Soma (Num 4) (Num 5)) (Num 12)

prog1 :: Exp
prog1 = Ap (IF TRUE (Fun "x" INT (Soma (Var "x") (Num 1))) (Fun "x" INT (Soma (Var "x") (Num 2)))) (Num 2)

-- > (if True then (Fun x:Int in x + 1) (Fun x:Int in x+2) 2
-- Resp: 3


prog2 :: Exp
prog2 = (Let "x" (F INT INT) (Fun "x" INT (Soma (Var "x") (Num 1))) (Ap (Var "x") (Num 10)))

-- > (Let x: Int -> Int = (fun x : Int => x + 1) in x 10
-- Resp: 11


prog3 :: Exp
prog3 = Fun "f1" (F INT INT) (Fun "y" BOOL (Soma (Num 4) (Ap (Var "f1") (Num 1))))

-- > fun f1 : Int -> Int => (fun y: Bool => 4 + (f1 1))

prog4 :: Exp
prog4 = Let "x" (F INT INT) (Fun "x" INT (Soma (Var "x") (Num 1))) (Var "x")
