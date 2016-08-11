{-# LANGUAGE DeriveFunctor #-}

-- Fix point combinator
newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

-- sumtype for the expression node types
data ExprF r = Const Integer
             | Var Char
             | Add r    r
             | Sub r    r
             | Abs Char r
             | App r    r
    deriving Functor

data EvalF f r = EvalF {
    expr  :: f r,                    -- the expression being evaluated
    stack :: [Fix (EvalF f)],        -- the expressions coming from function applications (later bindings at the end)
    env   :: [(Char, Fix (EvalF f))] -- arguments already bound by lambdas
} deriving Functor

type EvalExprF = EvalF ExprF

-- catamorphism
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

-- anamorphism
ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

-- algebra for evaluating an expression tree
alg :: EvalExprF Integer -> Integer
alg (EvalF x _ _) = case x of
					Const i   -> i
					Var   c   -> error "undefined variable" -- 0 --should not encounter!
					Add   x y -> x + y
					Sub   x y -> x - y
					Abs   s b -> b
					App   f v -> f

--shorthand for our evaluator
eval :: Fix EvalExprF -> Integer
eval = cata alg

coalg :: Fix EvalExprF -> EvalExprF (Fix EvalExprF)
coalg (Fix (EvalF x stack env)) = case x :: ExprF (Fix EvalExprF) of
                                        Const i    -> EvalF (Const i) [] []
                                        
                                        -- searches the env for the character 'c' and replace var with the bound expression
                                        Var   c    -> let EvalF x _ _ = ( unFix $ snd (head (filter (\x -> ((fst x) == c)) env) ) ) in
																		EvalF (Abs '0' (Fix $ (EvalF x stack env))) [] []
                                        -- unpack the expressions and pass on stack and env
                                        Add   ex1 ex2 -> let EvalF x1 _ _ = (unFix ex1)
                                                             EvalF x2 _ _ = (unFix ex2) in EvalF (Add (Fix $ (EvalF x1 stack env)) (Fix $ (EvalF x2 stack env))) [] []
                                        Sub   ex1 ex2 -> let EvalF x1 _ _ = (unFix ex1)
                                                             EvalF x2 _ _ = (unFix ex2) in EvalF (Sub (Fix $ (EvalF x1 stack env)) (Fix $ (EvalF x2 stack env))) [] []
                                                             
                                        -- take first (earliest) expression from the stack and add to the env with the current symbol, nested expressions receive the rest of the stack
                                        Abs   c   ex2 -> let EvalF x2 _ _ = (unFix ex2) in EvalF (Abs c (Fix $ (EvalF x2 (tail stack) (env++[(c, head stack)]) ))) [] []
                                        
                                        -- add the argument (right node) to the stack, the argument stores the earlier stage of bindings
                                        App   ex1 ex2 -> let EvalF x1 _ _ = (unFix ex1)
                                                             EvalF x2 _ _ = (unFix ex2) in EvalF (App (Fix $ (EvalF x1 (stack++[Fix $ (EvalF x2 stack env)]) env)) (Fix $ (EvalF (Const 0) [] []))) [] [] 



-- shorthand for our name binder
pre :: (Fix EvalExprF) -> (Fix EvalExprF)
pre = ana coalg

-- helper for constructing integer constants
int :: Integer -> Fix EvalExprF
int i = Fix $ EvalF (Const i) [] []

-- helper for constructing variables
var :: Char -> Fix EvalExprF
var c = Fix $ EvalF (Var c) [] []

-- helper for constructing applications
app :: Fix EvalExprF -> Fix EvalExprF -> Fix EvalExprF
app f x = Fix $ EvalF (App f x) [] []

-- helper for constructing lambda abstractions
la :: Char -> Fix EvalExprF -> Fix EvalExprF
la c b = Fix $ EvalF (Abs c b) [] []

-- helper for constructing additions
add :: Fix EvalExprF -> Fix EvalExprF -> Fix EvalExprF
add f x = Fix $ EvalF (Add f x) [] []

-- helper for constructing subtraction
sub :: Fix EvalExprF -> Fix EvalExprF -> Fix EvalExprF
sub f x = Fix $ EvalF (Sub f x) [] []

-- test expressions
e1 = app
		(app
			(la 'x' (la 'y' ( sub (var 'x') (var 'y') ) ) )
			(int 8) )
			(int 50)
		
e2 = app
		(la
			'f'
			(app
				(var 'f')
				(int 5)))
		(la 'x' (add (var 'x') (var 'x')) )

e3 = app (la 'x' (app (var 'x') (var 'x'))) (la 'y' (app (var 'y') (var 'y')))

main =    print $ eval $ pre $ e3