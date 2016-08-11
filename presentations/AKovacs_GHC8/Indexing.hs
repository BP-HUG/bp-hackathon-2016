{-# language
    AllowAmbiguousTypes
  , DeriveTraversable
  , GADTs
  , MagicHash
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , TypeOperators
  #-}

module Indexing where

import qualified Data.Vector as V
import Data.Vector (Vector)
import GHC.TypeLits
import Data.Kind

import Data.Singletons.Prelude
import Data.Singletons.TypeLits

import Data.Constraint
import GHC.Prim

{- Simple sized arrays using wrappers, smart constructors and hidden internals -}

newtype Array (n :: Nat) (a :: Type) = Array (Vector a)
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

natToInt :: forall (n :: Nat). SingI n => Int
natToInt = fromInteger $ fromSing (sing :: Sing n)

new :: forall (n :: Nat) a. SingI n => a -> Array n a
new a = Array (V.replicate (natToInt @n) a)

-- :t new @10 True

index ::
  forall (i :: Nat) (n :: Nat) a . (SingI i, i + 1 <= n) =>
  Array n a -> a
index (Array v) = V.unsafeIndex v (natToInt @i)

-- index @0 (new @10 True)
-- index @10 (new @10 True) -- bound check fails

--------------------------------------------------------------------------------

-- But we would still like to have a safe API for unknown lenghts
-- First, we need to do typed bounds checking

decideLEQ :: Sing (x :: Nat) -> Sing (y :: Nat) -> Maybe (Dict (x <= y))
decideLEQ x y = case x %:<= y of
  STrue  -> Just (unsafeCoerce# (Dict :: Dict (() ~ ())))
  SFalse -> Nothing

-- API  

index' ::
  forall (i :: Nat) (n :: Nat) a. (SingI i, SingI n) =>
  Array n a -> Maybe a
index' arr = let
  si = sing :: Sing i
  sn = sing :: Sing n
  in case decideLEQ (si %:+ (sing :: Sing 1)) sn of
       Just Dict -> Just (index @i arr)
       _         -> Nothing
       

-- it's ugly here, but given a rich enough set of operations for
-- deciding things on Nat-s, we can build up rich and safe API-s.

-- For example: matrix library with potentially statically known
-- number of dimensions AND lengths in each dimension, where
-- operations modify the shape (as reflected in types) of a matrix
-- appropriately. 

