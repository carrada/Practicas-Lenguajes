-- Emilio Bocanegra Paniagua
-- Juan Diego Hernandez Sanchez
-- Carrada Rodriguez Cristopher Emiliano


data Elist a = L [(a,a)] deriving (Show, Eq)

lp1 = L [(1,2),(4,3),(4,5)]
lp2 = L [(2,4),(8,8),(7,0)]

length2 :: (Elist a) -> Int
length2 (L []) = 0
length2 (L (x:xs)) = 2 + length2 (L xs)

elem2 :: Eq a => (Elist a) -> a -> Bool
elem2  (L[]) a = False
elem2  (L ((x, y):xs)) a  = (a == x) || (a == y) || (elem2 (L (xs)) a)

append2 :: (Elist a) -> (Elist a) -> (Elist a)
append2 l (L[]) = l
append2 (L (x:xs)) (L y) = (L (x : (xs ++ y)))
