{-# language
    ConstraintKinds
  , DataKinds
  , GADTs
  , RankNTypes
  , ScopedTypeVariables
  , TypeFamilies
  , TypeInType
  , TypeOperators
  , UndecidableInstances
  #-}

import Data.Kind

----------------------------------------
-- Type In Type

{-

Old GHC :

  Four levels:

    1. Term level : most expressive, functions, bindings, case expressions

    2. Type level : classes, type families, lifted data, no functions, "let" or "case"

    3. Kind level : Lifted kinds, * and kind arrow, no functions, families or classes

    4. Sort level : A single sort called BOX

   Expressivity decreases when walking up the hierarchy.


Eventual goal & ideal :

  Depenent typing : an infinite ladder of levels, each level has the same expressive power.

  Advantages:
    - No ceiling to abstraction
    - No multiple languages with different semantics
        (e. g. type family vs functions)
    - Simpler core + greater expressive power
    - Lots of background research

  Disadvantages:
    - Mostly myths
        (e. g. worse type inference, no phase separation, less erasure)

-}


{-
What TypeInType does:

   Merges levels 2, 3, and 4 into one. Only terms and types now, types classify themselves
   Illustration:

   Terms
     |
   Types
     |
   Types
     |
    ... it's Types all the way up (down)


   Type :: Type inconsistent but appropriate for Haskell.
-}


-- :i Type
-- :kind (*)
-- :kind (Type :: Type)

-- kind synonym
type MTKind = (* -> *) -> * -> *

data Nat = Z | S Nat deriving (Show)

-- kind family
type family Arity (f :: Type) :: Nat where
  Arity (a -> b) = S (Arity b)
  Arity a        = Z

-- :kind! Arity (Int -> Int)
-- :kind! Arity MTKind


-- dependent kind parameters
data PolyProxy (t :: *) (x :: t) = PolyProxy

polyProxy1 :: PolyProxy * Int
polyProxy1 = PolyProxy

polyProxy2 :: PolyProxy (* -> *) []
polyProxy2 = PolyProxy

-- kind equality constraints (silly example)
kindConstraint :: ((*) ~ (*)) => Int
kindConstraint = 0

-- explicit forall in data definition
data Some :: forall (a :: *). (a -> *) -> * where
  Some :: f x -> Some f

 
-- Kind-indexed GADTs

data G (t :: *) (x :: t) where
  GInt   :: G * Int
  -- pattern matching on constructors discovers the index kind
  GMaybe :: G (* -> *) Maybe
      
-- kind class :
--   simple singleton class definition with good inference
--   not workable yet due to to non-lifting of primitive types

class SingType (t :: *) where
  data Sing :: t -> *                     -- t used as kind
  fromSing  :: Sing (x :: t) -> t         -- t used as kind and type
  toSing    :: t -> Some (Sing :: t -> *)

{-
What TypeInType does:

     Streghtens type level: makes most GADTs liftable

     Not only type-level programming, but type-safe type-level programming

     Type language is effectively full-dependent (but not very good at being such)

     program with types, run with :kind! -- who needs values?

-}

-- Some type-level proofs on lifted homogeneous equality
data (==) :: forall (t :: *). t -> t -> * where
  Refl :: x == x -- pattern matching on Refl discovers that the indices are equal

-- congruence
type family Cong (f :: a -> b) (x :: a) (y :: a) (eq :: x == y) :: f x == f y where
  Cong f x x Refl = Refl -- note the non-linear "x"!!

-- symmetry
type family Sym (eq :: (x :: a) == (y :: a)) :: y == x where   -- implicit "a" parameter
  Sym Refl = Refl


-- Length-indexed vectors as usual

data Vec :: Nat -> * -> * where
  Nil  :: Vec Z a
  (:>) :: a -> Vec n a -> Vec (S n) a
infixr 5 :>

-- type-safe type-level "head" for type-level vectors

type family Head (xs :: Vec (S n) a) :: a where
  Head (x :> xs) = x

-- :kind! Head (Int :> Nil)
-- :kind! Head Nil


-- Safe concatentation on the type level

type family (a :: Nat) + (b :: Nat) :: Nat where
  Z   + b = b
  S a + b = S (a + b)

type family (xs :: Vec n a) ++ (ys :: Vec m a) :: Vec (n + m) a where
  Nil       ++ ys = ys
  (x :> xs) ++ ys = x :> (xs ++ ys)

type V1 = Int :> Bool :> Nil
type V2 = () :> Vec Z Int :> Nil
type V3 = V1 ++ V2  -- :kind! V3


{-
HOWEVER:

   - No "let", "case", or lambdas on type level. (intermezzo: STLC)

   - Primitive types can't be lifted (Int#, Char#, String, etc..)

   - The GHC.TypeLits situtation is a hot mess.

   - Classes and constraints can't be lifted.

   - Type family evaluation is very slow.

   - A number of bugs and limitations.


How it's useful?

   - Mostly useful as first step towards more dependent Haskell

   - More powerful singletons and generics

-}


