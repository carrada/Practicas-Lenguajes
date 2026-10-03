--INTEGRANTES:
-Emilio Bocanegra Paniagua
-Juan Diego Hernández Becerril
-Cristopher Emiliano Carrada
type ID = String
data EAB = Num Int | Var ID | Bool Bool
         | Suma EAB EAB | Prod EAB EAB
         | Suc EAB | Pred EAB
         | Not EAB
         | If EAB EAB EAB
         | IsZero EAB
         | Lt EAB EAB | Gt EAB EAB | Eq EAB EAB
         | Let ID EAB EAB
           deriving (Eq)

type Env = [( ID , EAB )]


-- 1. Clase Show
instance Show EAB where
    show (Num n)        = show n
    show (Var x)        = x
    show (Bool True)    = "true"
    show (Bool False)   = "false"
    show (Suma a b)     = "(" ++ (show a) ++ "+" ++ (show b) ++ ")"
    show (Prod a b)     = "(" ++ (show a) ++ "*" ++ (show b) ++ ")"
    show (Lt a b)       = "(" ++ show a ++ "<" ++ show b ++ ")"
    show (Gt a b)       = "(" ++ show a ++ ">" ++ show b ++ ")"
    show (Eq a b)       = "(" ++ show a ++ "==" ++ show b ++ ")"
    show (Suc a)        = "suc("++ show a ++ ")"
    show (Pred a)       = "pred("++ show a ++ ")"
    show (Not a)        = "not("++ show a ++ ")"
    show (IsZero a)     = "isZero("++ show a ++ ")"
    show (If a b c)     = "(if " ++ show a ++ " then " ++ show b ++ " else " ++ show c ++ ")"
    show (Let x a b)    = "(let " ++ x ++ " = " ++ show a ++ " in " ++ show b ++ ")"


-- 2. Función eval
eval :: Env -> EAB -> Either Int Bool
eval _ (Num n)              = Left n
eval _ (Bool b)             = Right b
eval env (Var x)            = case lookup x env of
                                   Just e   -> eval env e
                                   Nothing  -> error "Variable no encontrada"
eval env (Suma a b)         = case (eval env a, eval env b) of
                                   (Left a1, Left b1) -> Left (a1 + b1)
                                   _                  -> error "Error de tipos"
eval env (Prod a b)         = case (eval env a, eval env b) of
                                   (Left a1, Left b1) -> Left (a1 * b1)
                                   _                  -> error "Error de tipos"
eval env (Suc a)            = case (eval env a) of
                                   (Left a1) -> Left (1 + a1)
                                   _         -> error "Error de tipos"
eval env (Pred a)           = case (eval env a) of
                                   (Left a1) -> Left (a1 - 1)
                                   _         -> error "Error de tipos"
eval env (Not a)            = case (eval env a) of
                                   (Right True)     -> Right False
                                   (Right False)    -> Right True
                                   _                -> error "Error de tipos"
eval env (If cond a b)      = case (eval env cond) of
                                   (Right True)     -> eval env a
                                   (Right False)    -> eval env b
                                   _                -> error "Error de tipos"
eval env (IsZero a)         = case (eval env a) of
                                   (Left a1) -> Right (a1 == 0)
                                   _         -> error "Error de tipos"
eval env (Lt a b)           = case (eval env a, eval env b) of
                                   (Left a1, Left b1) -> Right (a1 < b1)
                                   _                  -> error "Error de tipos"
eval env (Gt a b)           = case (eval env a, eval env b) of
                                   (Left a1, Left b1) -> Right (a1 > b1)
                                   _                  -> error "Error de tipos"
eval env (Eq a b)           = case (eval env a, eval env b) of
                                   (Left a1, Left b1) -> Right (a1 == b1)
                                   _                  -> error "Error de tipos"
eval env (Let x a b)        = eval (env ++ [(x, a)]) b

-- 3. Función sust
sust :: ID -> EAB -> EAB -> EAB
sust x e (Num n) = Num n
sust x e (Var var)
    | var == x = e
    | otherwise = Var var
sust x e (Bool b) = Bool b
sust x e (Suma a b) = Suma (sust x e a) (sust x e b)
sust x e (Prod a b) = Prod (sust x e a) (sust x e b)
sust x e (Suc a) = Suc (sust x e a)
sust x e (Pred a) = Pred (sust x e a)
sust x e (Not a) = Not (sust x e a)
sust x e (If a b c) = If (sust x e a) (sust x e b) (sust x e c)
sust x e (Let a b c)
    | x == a = Let a (sust x e b) c
    | aEstaEnE && xEstaEnC = error "Captura de variable libre"
    | otherwise = Let a (sust x e b) (sust x e c)
    where
        aEstaEnE = sust a (Var "var") e /= e
        xEstaEnC = sust x (Var "var") c /= c

--Funcion evalStep

evalStep :: EAB -> EAB
evalStep (Num n) = Num n
evalStep (Bool b) = Bool b
evalStep (Var x) = Var x
evalStep (Suma (Num n1) (Num n2)) = Num (n1 + n2)
evalStep (Suma v1@(Num _) e2) = Suma v1 (evalStep e2)
evalStep (Suma e1 e2) = Suma (evalStep e1) e2
evalStep (Prod (Num n1) (Num n2)) = Num (n1 * n2)
evalStep (Prod v1@(Num _) e2) = Prod v1 (evalStep e2)
evalStep (Prod e1 e2) = Prod (evalStep e1) e2
evalStep (Suc (Num n)) = Num (n + 1)
evalStep (Suc e) = Suc (evalStep e)
evalStep (Pred (Num n)) = Num (n - 1)
evalStep (Pred e) = Pred (evalStep e)
evalStep (Not (Bool True)) = Bool False
evalStep (Not (Bool False)) = Bool True
evalStep (Not e) = Not (evalStep e)
evalStep (IsZero (Num 0)) = Bool True
evalStep (IsZero (Num _)) = Bool False
evalStep (IsZero e) = IsZero (evalStep e)
evalStep (Lt (Num n1) (Num n2)) = Bool (n1 < n2)
evalStep (Lt v1@(Num _) e2) = Lt v1 (evalStep e2)
evalStep (Lt e1 e2) = Lt (evalStep e1) e2
evalStep (Gt (Num n1) (Num n2)) = Bool (n1 > n2)
evalStep (Gt v1@(Num _) e2) = Gt v1 (evalStep e2)
evalStep (Gt e1 e2) = Gt (evalStep e1) e2
evalStep (Eq (Num n1) (Num n2)) = Bool (n1 == n2)
evalStep (Eq (Bool b1) (Bool b2)) = Bool (b1 == b2)
evalStep (Eq v1@(Num _) e2) = Eq v1 (evalStep e2)
evalStep (Eq v1@(Bool _) e2) = Eq v1 (evalStep e2)
evalStep (Eq e1 e2) = Eq (evalStep e1) e2
evalStep (If (Bool True) e1 _) = e1
evalStep (If (Bool False) _ e2) = e2
evalStep (If cond e1 e2) = If (evalStep cond) e1 e2
evalStep (Let x (Num n) e2) = sust x (Num n) e2
evalStep (Let x (Bool b) e2) = sust x (Bool b) e2
evalStep (Let x e1 e2) = Let x (evalStep e1) e2



--Funcion evalDin
evalDin :: EAB -> EAB
evalDin e =
    let e' = evalStep e
    in if e == e'
       then e           -- Estado bloqueado o valor final alcanzado
       else evalDin e'  -- Si hubo un cambio, damos otro paso recursivamente


-- Función isValid
-- Evalúa la expresión al máximo y comprueba si el estado final es un valor.
isValid :: EAB -> Bool
isValid e = case evalDin e of
           Num _  -> True
           Bool _ -> True
           _      -> False

-- ==========================================
-- 3 Semántica Estática
-- ==========================================

data Type = Nat | Boolean deriving (Eq, Show)
type Ctx = [(ID, Type)]

typeEAB :: Ctx -> EAB -> Type
typeEAB _ (Num _) = Nat
typeEAB _ (Bool _) = Boolean

typeEAB ctx (Var x) = case lookup x ctx of
    Just t  -> t
    Nothing -> error ("Variable libre: " ++ x)

typeEAB ctx (Suma e1 e2) =
    case (typeEAB ctx e1, typeEAB ctx e2) of
        (Nat, Nat) -> Nat
        (t1, _) | t1 /= Nat -> error ("Expected Number: (" ++ show e1 ++ ")")
        _ -> error ("Expected Number: (" ++ show e2 ++ ")")

typeEAB ctx (Prod e1 e2) =
    case (typeEAB ctx e1, typeEAB ctx e2) of
        (Nat, Nat) -> Nat
        (t1, _) | t1 /= Nat -> error ("Expected Number: (" ++ show e1 ++ ")")
        _ -> error ("Expected Number: (" ++ show e2 ++ ")")

typeEAB ctx (Suc e) =
    if typeEAB ctx e == Nat then Nat
    else error ("Expected Number: (" ++ show e ++ ")")

typeEAB ctx (Pred e) =
    if typeEAB ctx e == Nat then Nat
    else error ("Expected Number: (" ++ show e ++ ")")

typeEAB ctx (IsZero e) =
    if typeEAB ctx e == Nat then Boolean
    else error ("Expected Number: (" ++ show e ++ ")")

typeEAB ctx (Not e) =
    if typeEAB ctx e == Boolean then Boolean
    else error ("Expected Boolean: (" ++ show e ++ ")")

typeEAB ctx (Lt e1 e2) =
    case (typeEAB ctx e1, typeEAB ctx e2) of
        (Nat, Nat) -> Boolean
        (t1, _) | t1 /= Nat -> error ("Expected Number: (" ++ show e1 ++ ")")
        _ -> error ("Expected Number: (" ++ show e2 ++ ")")

typeEAB ctx (Gt e1 e2) =
    case (typeEAB ctx e1, typeEAB ctx e2) of
        (Nat, Nat) -> Boolean
        (t1, _) | t1 /= Nat -> error ("Expected Number: (" ++ show e1 ++ ")")
        _ -> error ("Expected Number: (" ++ show e2 ++ ")")

typeEAB ctx (Eq e1 e2) =
    if typeEAB ctx e1 == typeEAB ctx e2 then Boolean
    else error ("Type mismatch in Eq: no coinciden los tipos")

typeEAB ctx (If e1 e2 e3) =
    if typeEAB ctx e1 == Boolean then
        let t2 = typeEAB ctx e2
            t3 = typeEAB ctx e3
        in if t2 == t3 then t2
           else error ("Type mismatch in If branches: las ramas difieren")
    else error ("Expected Boolean: (" ++ show e1 ++ ")")

typeEAB ctx (Let x e1 e2) =
    typeEAB ((x, typeEAB ctx e1):ctx) e2


evalEst :: EAB -> Either Int Bool
evalEst e = 
    let _validarTipado = typeEAB [] e
    in eval [] e