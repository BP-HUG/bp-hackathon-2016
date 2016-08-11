{-# language
    AllowAmbiguousTypes
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , MultiParamTypeClasses
  , PartialTypeSignatures
  , RankNTypes
  , ScopedTypeVariables
  , StandaloneDeriving
  , TemplateHaskell
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , TypeOperators
  #-}

import qualified GHC.TypeLits as Lit
import GHC.TypeLits (type (<=))
import Data.Kind
import Control.Lens hiding ((:>), has, Fold)
import Data.Constraint

import Data.Singletons.Prelude
import Data.Nat

import qualified Data.Vector as V
import Data.Vector (Vector)

{-
EXPLICIT TYPE APPLICATIONS

I think it's pretty important, because

    1. TypeApplications makes Proxy unnecessary.

    2. Proxy was so ugly that it deterred people from many perfectly
       good use cases of type-level programming.

-}

-- Basic example
-- id @Int

-- implicit params in the order of appearance
-- const @Int

-- Can specify param order
const' :: forall a. a -> forall b. b -> a
const' x y = x
-- const' True @Int

-- Specify only n-th parameter:
-- const' @_ @Int

-- class parameters
class Test (a :: *) where
  test :: a -> a

instance Test Int where
  test = id


-- Example 1 : non-ugly records
--------------------------------------------------------------------------------

data Rec :: [(Symbol, Type)] -> Type where
  End  :: Rec '[]
  Add :: forall (s :: Symbol) t ts. t -> Rec ts -> Rec ('(s , t) ': ts)

r1 = Add @"name"   "John" $
     Add @"age"    20     $
     Add @"height" 100    $
     End

-- Alternatively with HLists

data HList :: [Type] -> Type where
  HNil  :: HList '[]
  (::>) :: t -> HList ts -> HList (t ': ts)
infixr 5 ::>  

data (:=) (name :: Symbol) (t :: Type) where
  Field :: forall name t. t -> name := t
infixr 6 :=

r2 = Field @"name" "John" ::>
     Field @"age" 20 ::>
     HNil

-- Lots of possible features and machinery (see for example Vinyl)


-- Example 2 : overloaded lenses & traversals (maybe a bit too overloaded)
--------------------------------------------------------------------------------

class Has (n :: Symbol) (f :: * -> *) s t a b
  | n s -> a, n t -> b, n s b -> t, n t a -> s where
  has :: LensLike f s t a b

data Foo = Foo {_fooName :: String, _fooAge :: Int } deriving (Show)
data Bar = Bar1 {_barName :: String} | Bar2 {_barAge :: Int} deriving (Show)

makeLenses ''Foo
makeLenses ''Bar

instance Functor f => Has "name" f Foo Foo String String where
  has = fooName -- lens

instance Functor f => Has "age" f Foo Foo Int Int where
  has = fooAge  -- lens

instance Applicative f => Has "name" f Bar Bar String String where
  has = barName -- traversal

instance Applicative f => Has "age" f Bar Bar Int Int where
  has = barAge -- traversal  

a = Foo "foo" 0 ^. has @"name"
b = Bar1 "bar" ^? has @"name"

f x = x & has @"name" .~ ""
        & has @"age" +~ 10


-- Example 3 : curried generic functions
--------------------------------------------------------------------------------

-- See GenericFold.hs


-- Example 4 : Indexing
--------------------------------------------------------------------------------

-- See Indexing.hs



-- Example 5 : strongly typed variables in EDSL-s
--------------------------------------------------------------------------------

-- Shallowly embedded lambda calculus with Symbol variables

type Con = [(Symbol, *)] -- kind synonym

data Term :: Con -> * -> * where
  Var  :: forall v con t. Lookup v con ~ Just t => Term con t
  Lam  :: forall v con a b. Term ('(v , a) ': con) b -> Term con (a -> b)
  (:$) :: forall con a b. Term con (a -> b) -> Term con a -> Term con b

infixl 8 :$

deriving instance Show (Term con a)

-- the order of type params is a bit wrong though
-- :t Var @_ @"foo"

-- so let's use flipped "smart" constructors:

var :: forall v con t. Lookup v con ~ Just t => Term con t
var = Var @con @t @v

lam :: forall v con a b. Term ('(v , a) ': con) b -> Term con (a -> b)
lam = Lam @con

id'     = lam @"x" $ var @"x"
const'' = lam @"x" $ lam @"y" $ var @"x"
compose = lam @"f" $ lam @"g" $ lam @"x" $ var @"f" :$ (var @"g" :$ var @"x")

-- Exercise: write "eval :: forall con t. Term con t -> t" without unsafe functions
-- Hint: we'll need an alternative Lookup implementation

