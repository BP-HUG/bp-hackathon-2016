{-# LANGUAGE DeriveFunctor #-}

--Fix point combinator
newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

--Sumtype for the expression node types
data ExprF r = Const Integer 
             | Add r    r
             | Sub r    r
             | Mul r    r
             | Pow r    r
    deriving (Show, Functor)
    
type Expr = Fix ExprF

--catamorphism
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

--anamorphism
ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

showF :: Expr -> [Char]
showF = cata alg
    where
        alg (Const x) = show x
        alg (Add x y) = "(" ++ x ++ "+" ++ y ++ ")"
        alg (Sub x y) = "(" ++ x ++ "-" ++ y ++ ")"
        alg (Mul x y) = "(" ++ x ++ "*" ++ y ++ ")"
        alg (Pow x y) = "(" ++ x ++ "^" ++ y ++ ")"
        
intLog :: Integer -> Integer -> Integer
intLog base n = floor $ logBase (fromIntegral base) (fromIntegral n)
       
isPow :: Integer -> Integer -> Bool
isPow base x = let n0 = logBase (fromIntegral base) (fromIntegral x) in (floor n0) == (ceiling n0)

data WantRec = Rec | NoRec

toPowExpr :: Integer -> Integer -> Expr
toPowExpr base x = ana coAlg (x, Rec)
   where
     coAlg :: (Integer, WantRec) -> ExprF (Integer, WantRec)
     coAlg (n, NoRec)             = Const n
     coAlg (n, _) | n <= base     = Const n
     coAlg (n, _) | isPow base n  = Pow (base, NoRec) (intLog base n, NoRec)
     coAlg (n, _) | otherwise     = let ln = intLog base n in Add (base^ln, Rec) (n - (base^ln), Rec)
        
main = mapM_ (putStrLn . showF . toPowExpr 4) [7, 8, 9, 10, 11, 12, 13, 14, 15, 16]