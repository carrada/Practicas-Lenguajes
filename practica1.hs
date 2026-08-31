-- Emilio Bocanegra Paniagua
-- Juan Diego Hernandez Sanchez
-- Carrada Rodriguez Cristopher Emiliano


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
