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
  , OverloadedLabels
  , TemplateHaskell
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , TypeOperators
  #-}

module OverloadedLabels where

import Data.Kind
import Control.Lens hiding (has)
import GHC.TypeLits hiding (type (*))
import GHC.OverloadedLabels
import GHC.Prim
import Data.Proxy

{-
Overloaded labels let us substitute our code for each #label occurrence,
given "label" as a type-level Symbol.

  class IsLabel (x :: Symbol) a where
     fromLabel :: Proxy# x -> a

Doc says:

  "During typechecking, GHC will replace an occurrence of an
  overloaded label like #foo with  fromLabel (proxy# :: Proxy# "foo")"

-}

data Foo = Foo {_fooName :: String, _fooAge :: Int } deriving (Show)
data Bar = Bar1 {_barName :: String} | Bar2 {_barAge :: Int} deriving (Show)

makeLenses ''Foo
makeLenses ''Bar

-- Note that this implies that for each name and "s" there's only one "a"
instance (a ~ String, Functor f) => IsLabel "name" (LensLike' f Foo a) where
  fromLabel _ = fooName

instance (a ~ Int, Functor f) => IsLabel "age" (LensLike' f Foo a) where
  fromLabel _ = fooAge

instance (a ~ String, Applicative f) => IsLabel  "name" (LensLike' f Bar a) where
  fromLabel _ = barName

a = Foo "foo" 0 ^. #name
b = Bar1 "bar" ^? #name
c = Foo "foo" 0 ^. #age
  

-- -- Alternatively, we could use OverloadedLabels as a general-purpose
-- -- mechanism to shave off two characaters from Symbol type applications

-- -- Convert all labels to Proxy
-- instance (psym ~ Proxy sym) => IsLabel sym psym where
--   fromLabel _ = Proxy
 
-- -- Slightly modified stuff from TypeApp.hs
-- class (IsLabel n (Proxy n)) => Has (n :: Symbol) (f :: * -> *) s t a b
--   | n s -> a, n t -> b, n s b -> t, n t a -> s where
--   has :: Proxy n -> LensLike f s t a b

-- instance Functor f => Has "name" f Foo Foo String String where
--   has _ = fooName -- lens

-- instance Applicative f => Has "name" f Bar Bar String String where
--   has _ = barName -- traversal

-- -- And voila  
-- a = Foo "foo" 0 ^. has #name
-- b = Bar1 "bar" ^? has #name

-- f x = x & has #name .~ ""
--         & has #age +~ 10

-- -- Similarly with records

-- data Rec :: [(Symbol, Type)] -> Type where
--   End  :: Rec '[]
--   Add  :: Proxy s -> t -> Rec ts -> Rec ('(s , t) ': ts)

-- r1 = Add #name   "John" $
--      Add #age    20     $
--      Add #height 100    $
--      End

-- -- :t r1
