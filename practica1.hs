-- Emilio Bocanegra Paniagua
-- Juan Diego Hernández Becerril
-- Carrada Rodriguez Cristopher Emiliano

{-
1. Explique por qué es imposible generar una lista de longitud impar con esta implementación.

Respuesta:
Es imposible generar una lista de longitud impar debido a que la estructura `EList` está definida como una lista de pares o tuplas de dos elementos: [(a,a)]
Cada vez que se agrega un elemento a la lista interna de Haskell, esta agregando a fuerzas dos valores del tipo `a` al mismo tiempo. Si la lista interna tiene k tuplas, la lista EList va a tener 2k elementos.
Osea que en Haskell no es posible tener una tupla incompleta o de un solo elemento cuando el tipo exige (a,a), la longitud total siempre será un número par).
en resumen, la estructura misma hace imposible representar una cantidad impar de elementos.
-}

data Elist a = L [(a,a)] deriving (Show, Eq)

lp1 = L [(1,2),(3,4),(9,8)]
lp2 = L [(0,1),(3,7),(8,6),(5,4)]

length2 :: (Elist a) -> Int
length2 (L []) = 0
length2 (L (x:xs)) = 2 + length2 (L xs)

elem2 :: Eq a => (Elist a) -> a -> Bool
elem2  (L[]) a = False
elem2  (L ((x, y):xs)) a  = (a == x) || (a == y) || (elem2 (L (xs)) a)

append2 :: (Elist a) -> (Elist a) -> (Elist a)
append2 l (L[]) = l
append2 (L (x:xs)) (L y) = (L (x : (xs ++ y)))

reverse2 :: (Elist a) -> (Elist a)
reverse2 (L []) = (L [])
reverse2 (L ((a,b) : xs)) = append2 (reverse2 (L xs)) (L [(b,a)])

head2 :: (Elist a) -> a
head2 (L ((a,b): xs)) = a

map2 :: (a -> b) -> (Elist a) -> (Elist b)
map2 f (L []) = L []
map2 f (L((a,b):xs)) = append2 (L [(f a, f b)]) (map2 f (L xs))

cons2 :: a -> a -> (Elist a) -> (Elist a)
cons2 e1 e2 l = append2 (L [(e1,e2)]) l

snoc2 :: (Elist a) -> a -> a -> (Elist a)
snoc2 l e1 e2 = append2 l (L [(e1,e2)])

at2 :: (Elist a) -> Int -> a
at2 (L ((a,b):xs)) 1 = a
at2 (L ((a,b):xs)) 2 = b
at2 (L (x:xs)) int = at2 (L xs) (int-2)

update2 :: (Elist a) -> Int -> a -> (Elist a)
update2 (L((a,b):xs)) 1 e = (L((e,b):xs))
update2 (L((a,b):xs)) 2 e = (L((a,e):xs))
update2 (L(x:xs)) int e = append2 (L [x]) (update2 (L xs) (int-2) e)

replicate2 :: Int -> a -> EList a
replicate2 k x = L (replicate (k `div` 2) (x, x))

delfront2 :: EList a -> EList a
delfront2 (L (_ : xs)) = L xs

-- Elimina los dos últimos elementos
delrear2 :: Seq2k a -> Seq2k a
delrear2 (Seq2k xs) = Seq2k (eliminarFinal xs)
  where
    eliminarFinal [] = []
    eliminarFinal [_] = []
    eliminarFinal [_, _] = []
    eliminarFinal (y:ys) = y : eliminarFinal ys

delcenter2 :: EList a -> EList a
delcenter2 (L xs) = L (toPairs (take (k - 1) flatList ++ drop (k + 1) flatList))
  where
    -- k es la cantidad de pares originales (la mitad del total de elementos)
    k = length xs

    -- aplanar convierte la lista de tuplas en una lista de elementos individuales
    aplanar [] = []
    aplanar ((a,b):ys) = a : b : aplanar ys

    -- aca guardamos la lista aplanada para no recalcularla
    flatList = aplanar xs

    -- toPairs lo que hace es que convierte una lista de elementos individuales de vuelta a tuplas
    toPairs [] = []
    toPairs (y1:y2:ys) = (y1, y2) : toPairs ys

-- Elimina el primer y el último elemento
delext2 :: Seq2k a -> Seq2k a
delext2 (Seq2k xs) = 
    case xs of
        []     -> Seq2k []
        [_]    -> Seq2k []
        (_:ys) -> Seq2k (init ys)

-- Convierte la secuencia personalizada a una lista estándar
flat2 :: Seq2k a -> [a]
flat2 (Seq2k xs) = xs

-- Convierte una lista estándar a la secuencia personalizada
toL2 :: [a] -> Seq2k a
toL2 xs = Seq2k xs