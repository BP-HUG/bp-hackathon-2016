{-# LANGUAGE DeriveFunctor #-}

--Fix point combinator
newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

--Sumtype for the expression node types
data ExprF r = Const Integer 
             | Add r    r
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
        alg (Mul x y) = "(" ++ x ++ "*" ++ y ++ ")"
        alg (Pow x y) = "(" ++ x ++ "^" ++ y ++ ")"
        
evalF :: Expr -> Integer
evalF = cata alg
    where
        alg (Const x) = x
        alg (Add x y) = x+y
        alg (Mul x y) = x*y
        alg (Pow x y) = x^y
        
con x   = Fix $ Const x
add x y = Fix $ Add x y
mul x y = Fix $ Mul x y
pow x y = Fix $ Pow x y
        
main = print $ evalF $ mul (pow (con 2) (add (con 4) (con 3))) (con 2)