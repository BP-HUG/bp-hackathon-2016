{-# LANGUAGE DeriveFunctor #-}

--Fix point combinator
newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

--Sumtype for the expression node types
data ExprF r = Const Integer
             | Add r r
             | Mul r r
             | Pow r r
    deriving (Show, Functor)
    
data WithCostF f r = WithCost (f r) Integer
    deriving Functor

type ExprWithCostF = WithCostF ExprF

cost :: WithCostF f r -> Integer
cost (WithCost _ c) = c

--catamorphism
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

--anamorphism
ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

calculateCost :: Fix ExprF -> Fix ExprWithCostF
calculateCost = cata alg
  where costF = cost . unFix
        alg :: ExprF (Fix ExprWithCostF) -> Fix ExprWithCostF
        alg (Const x  ) = Fix $ WithCost (Const x)    1 
        alg (Add x0 y0) = Fix $ WithCost (Add x0 y0) (1  + costF x0 + costF y0)
        alg (Mul x0 y0) = Fix $ WithCost (Mul x0 y0) (2  + costF x0 + costF y0)
        alg (Pow x0 y0) = Fix $ WithCost (Pow x0 y0) (10 + costF x0 + costF y0)

class PrettyPrintF f where
    prettyPrintF :: f String -> String

instance PrettyPrintF ExprF where
    prettyPrintF (Const x) = show x
    prettyPrintF (Add x y) = "(" ++ x ++ "+" ++ y ++ ")"
    prettyPrintF (Mul x y) = "(" ++ x ++ "*" ++ y ++ ")"
    prettyPrintF (Pow x y) = "(" ++ x ++ "^" ++ y ++ ")"

instance PrettyPrintF f => PrettyPrintF (WithCostF f) where
    prettyPrintF (WithCost x c) = prettyPrintF x ++ "[" ++ show c ++ "]"

con x   = Fix (Const x)
add x y = Fix (Add x y)
mul x y = Fix (Mul x y)
pow x y = Fix (Pow x y)
        
main = putStr $ "before: " ++ before ++ "\n" ++ "after: " ++ after ++ "\n"
  where e      = mul (pow (con 2) (add (con 4) (con 3))) (con 2)
        before = cata prettyPrintF e
        after  = cata prettyPrintF (calculateCost e)
