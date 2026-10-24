import qualified Data.Set as Set
import Data.Set (Set)

type ID = String

data LNLambda = Bvar Int | Fvar ID | AbsL LNLambda | AppL LNLambda LNLambda deriving Eq

type SubstL = (Int , ID )

type Subst = ( ID , LNLambda )

-- Instancia la clase Show para el tipo de dato LNLambda.
instance Show LNLambda where
    show (Bvar n)        = show n
    show (Fvar x)        = x
    show (AbsL l)        = "(\\." ++ show l ++ ")"
    show (AppL l1 l2)    = "(" ++ show l1 ++ " " ++ show l2 ++ ")"


-- Función varOpen e s que devuelve una copia de e en la que todas las variables ligadas que
-- se refieren a la abstracción más externa son remplazadas por la variable indicada en s.

varOpen :: LNLambda -> SubstL -> LNLambda
varOpen (Bvar n) (k, x)
    | n == k     = Fvar x
    | otherwise  = Bvar n
varOpen (Fvar y) _       = Fvar y
varOpen (AbsL l) (k, x)  = AbsL (varOpen l (k+1, x))
varOpen (AppL l1 l2) sus = AppL (varOpen l1 sus) (varOpen l2 sus)

-- Función varClose e s que devuelve una copia de e en la que todas las apariciones de la
-- variable indicada por s han sido reemplazadas por una variable ligada.

varClose :: LNLambda -> SubstL -> LNLambda
varClose (Bvar n) _          = Bvar n
varClose (Fvar y) (k, x)
    | y == x = Bvar k
    | otherwise = Fvar y
varClose (AbsL l) (k, x)     = AbsL (varClose l (k + 1, x))
varClose (AppL l1 l2) sus    = AppL (varClose l1 sus) (varClose l2 sus)

-- Función locClose e que devuelve True syss e es localmente cerrado. Investiga recursivamente
-- el término, abriendo cada abstracción con un nombre, y comprobando que nunca se alcanza ninguna
-- variable ligada.
locClose :: LNLambda -> Bool
locClose (Bvar _)        = False
locClose (Fvar _)        = True
locClose (AbsL l)        = locClose (varOpen l (0, "x"))
locClose (AppL l1 l2)    = locClose l1 && locClose l2

-- Función locCloseK e k que devuelve True syss e es localmente cerrado. Analiza variables
-- ligadas, para comprobar que cada variable ligada es menor que el número de abstracciones que la
-- encierran.
locCloseK :: LNLambda -> Int -> Bool
locCloseK (Bvar n) k        = n < k
locCloseK (Fvar x) k        = True
locCloseK (AbsL l) k        = locCloseK l (k + 1)
locCloseK (AppL l1 l2) k    = locCloseK l1 k && locCloseK l2 k

-- Función freeVar e que devuelve el conjunto de variables libres presentes en e.
freeVar :: LNLambda -> Set ID
freeVar (Bvar n)        = Set.empty
freeVar (Fvar x)        = Set.singleton x
freeVar (AbsL l)        = freeVar l
freeVar (AppL l1 l2)    = Set.union (freeVar l1) (freeVar l2)

-- Función fresh e x que devuelve True syss x es fresh respecto a e. Decimos que un nombre x
-- es fresh para un término t, si x no pertenece al conjunto de variables libres de t.

fresh :: LNLambda -> ID -> Bool
fresh (Bvar n) _        = True
fresh (Fvar y) x
    | x == y = False
    | otherwise = True
fresh (AbsL l) x        = fresh l x
fresh (AppL l1 l2) x    = fresh l1 x && fresh l2 x

-- Función closed e que devuelve True syss e no tiene ninguna variable libre.
closed :: LNLambda -> Bool
closed (Bvar n)     = True
closed (Fvar x)     = False
closed (AbsL l)     = closed l
closed (AppL l1 l2) = closed l1 && closed l2

-- Función subst e s que devuelve una copia de e aplicando la sustitución indicada por s.
subst :: LNLambda -> Subst -> LNLambda
subst (Bvar n) _        = Bvar n
subst (Fvar y) (x, s)
    | y == x = s
    | otherwise = Fvar y
subst (AbsL l) sus      = AbsL (subst l sus)
subst (AppL l1 l2) sus  = AppL (subst l1 sus) (subst l2 sus)

-- Función betaR e que devuelve el término resultante de aplicar tantas β-reducciones como sean posibles a e.
betaR :: LNLambda -> LNLambda
betaR e = case betaStep e of
               Just e' -> betaR e'
               Nothing -> e

betaStep :: LNLambda -> Maybe LNLambda
betaStep (AppL (AbsL body) arg) = Just (subst (varOpen body (0, "z")) ("z", arg))
betaStep (AppL l1 l2)           = case betaStep l1 of
                                       Just l1' -> Just (AppL l1' l2)
                                       Nothing  -> case betaStep l2 of
                                                        Just l2'    -> Just (AppL l1 l2')
                                                        Nothing     -> Nothing
betaStep (AbsL l)               = case betaStep l of
                                       Just l'  -> Just (AbsL l')
                                       Nothing  -> Nothing
betaStep _                      = Nothing

