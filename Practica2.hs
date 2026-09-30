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




