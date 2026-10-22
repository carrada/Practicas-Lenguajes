type ID = String
data LNLambda = Bvar Int | Fvar ID | AbsL LNLambda | AppL LNLambda LNLambda deriving Eq

instance Show LNLambda where
    show (Bvar n) = show n
    show (Fvar x) = x
    show (AbsL l) = "(\\." ++ show l ++ ")"
    show (AppL l1 l2) = "(" ++ show l1 ++ " " ++ show l2 ++ ")"
