{-# language
    GADTs
  , TypeFamilies
  , TypeInType
  , TypeOperators
  , UndecidableInstances
  #-}

import Data.Kind
import GHC.Prim
import Data.Proxy

{-
Strongly typed lambda calculus on the type level.
-}

-- Examples
--------------------------------------------------------------------------------


-- type synonyms for common de Bruijn indices
type V0 = VZ
type V1 = VS V0
type V2 = VS V1
type V3 = VS V2
type V4 = VS V3
type V5 = VS V4

type Identity = L $ V V0  -- \x -> x
type Constant = L $ L $ V V1 -- \x y -> x
type Compose  = L $ L $ L $ V V2 :$ (V V1 :$ V V0) -- \f g x -> f (g x)

-- :kind! Eval Identity @@ Int
-- :kind! Eval Constant @@ Int @@ Bool
-- :kind! :kind! Eval Compose @@ Con1 [] @@ Con1 Maybe @@ Int

-- Also, type level laziness and recursion
-- :kind! Eval Add @@ (S (S Z))
-- :kind! Eval Add @@ S (S Z) @@ S (S Z)

data Nat = Z | S Nat

type family Pred n where
  Pred (S n) = n

data PredS :: Nat ~> Nat
type instance PredS @@ n = Pred n

-- Add = fix $ \add a b -> case a of 0 -> b; a -> S (add (pred a) b)
type Add =
  Fix $ L $ L $
    Case (V V1) '[
      '(T Z ,  V V0),
      '(V V1,  T (Con1 S) :$ (T ForceS :$ V V2 :$ (T PredS :$ V V1) :$ V V0))
      ]

-- Mul = fix $ \mul a b -> case a of 0 -> a; a -> add b (mul (pred a) b)
type Mul =
  Fix $ L $ L $
    Case (V V1) '[
      '(T Z , T Z),
      '(V V1, Add :$ V V0 :$ (T ForceS :$ V V2 :$ (T PredS :$ V V1) :$ V V0))
      ]

--------------------------------------------------------------------------------

data Fun a b
type a ~> b = Fun a b -> Type
infixr 0 ~>

infixl 9 @@
type family (@@) (f :: a ~> b) (x :: a) :: b

data Con1 :: (a -> b) -> (a ~> b)
data Con2 :: (a -> b -> c) -> (a ~> b ~> c)
data Con3 :: (a -> b -> c -> d) -> (a ~> b ~> c ~> d)

type instance Con1 c @@ a = c a
type instance Con2 c @@ a = Con1 (c a)
type instance Con3 c @@ a = Con2 (c a)

infixr 0 $
type ($) f x = f x

data Var :: [Type] -> Type -> Type where
  VZ :: Var (t ': ts) t
  VS :: Var ts t -> Var (t' ': ts) t

data Term :: [Type] -> Type -> Type where
  T    :: t -> Term ts t
  V    :: Var ts t -> Term ts t
  L    :: Term (t ': ts) t' -> Term ts (t ~> t')
  Fix  :: Term (Delay t ': ts) t -> Term ts t
  (:$) :: Term ts (a ~> b) -> Term ts a -> Term ts b
  Let  :: Term ts t -> Term (t ': ts) t' -> Term ts t' -- only sugar
  Case :: Term ts t -> [(Term ts t, Term ts t')] -> Term ts t'
infixl 9 :$

data Env :: [Type] -> Type where
  ENil  :: Env '[]
  ECons :: t -> Env cxt -> Env (t ': cxt)

type family LookupVar (v :: Var cxt t) (env :: Env cxt) :: t where
  LookupVar VZ     (ECons x env) = x
  LookupVar (VS v) (ECons x env) = LookupVar v env

type family Eval (term :: Term '[] t) :: t where
  Eval term = Eval_ term ENil

type family Eval_ (term :: Term cxt t) (env :: Env cxt) :: t where
  Eval_ (T x)       env = x
  Eval_ (V v)       env = LookupVar v env
  Eval_ (f :$ x)    env = Eval_ f env @@ Eval_ x env
  Eval_ (L e)       env = LamS e env
  Eval_ (Fix e)     env = Fix_ e env
  Eval_ (Let x e)   env = Eval_ (L e :$ x) env
  Eval_ (Case e cs) env = Eval_ (Lookup (Eval_ e env) (EvalCases env cs)) env  

type family Lookup k xs where
  Lookup k ('(k , v) ': xs) = v
  Lookup k ('(k', v) ': xs) = Lookup k xs

type family EvalCases
  (env :: Env ts) (cs :: [(Term ts t, Term ts t')]) :: [(t, Term ts t')] where
  EvalCases env '[]             = '[]
  EvalCases env ('(c, e) ': cs) = '(Eval_ c env , e) ': EvalCases env cs 

type family Fix_ (e :: Term (Delay t ': cxt) t) (env :: Env cxt) :: t where
  Fix_ e env = Eval_ e (ECons ('Delay (FixS e) env) env)

data FixS :: Term (Delay t ': cxt) t -> Env cxt ~> t
type instance FixS e @@ env = Fix_ e env

data LamS :: Term (a ': cxt) b -> Env cxt -> a ~> b
type instance LamS e env @@ x = Eval_ e (ECons x env)

data Delay b where
  Delay :: (a ~> b) -> a -> Delay b

type family Force (x :: Delay t) :: t where
  Force ('Delay f a) = f @@ a

data ForceS :: Delay t ~> t
type instance ForceS @@ dt = Force dt

