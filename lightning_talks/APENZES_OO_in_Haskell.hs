{-# LANGUAGE ScopedTypeVariables #-}
module Store
    ( Store(store, save, load)
    , StoreObj
    , ResetStore(resetStore, reset)
    , ResetStoreObj
    ) where

import Data.IORef

-- github.com/andorp/oo-haskell

-- Object oriented programming is based on ad-hoc polimorphism
-- A class in OO languages has two interpretation
--  * a class serves as a type
--  * a class serves as a collection of methods (things...)
-- Haskell type classes are the way how we express ad-hoc polymorphism.

-- We need a class...
class Store s where
    -- constructor
    store :: a -> IO (s a)
    -- methods
    save  :: s a -> a -> IO ()
    load  :: s a -> IO a


-- It looks like an OO object...
data StoreObj a = StoreObj {
      save_ :: a -> IO ()
    , load_ :: IO a
    }

-- It looks like a factory method...
newStore :: a -> IO (StoreObj a)
newStore x = do
    storeRef <- newIORef x

    let saveImp y = writeIORef storeRef y

    let loadImp = readIORef storeRef

    return $ StoreObj saveImp loadImp


instance Store StoreObj where
    store = newStore
    save  = save_
    load  = load_


class Store s => ResetStore s where
    resetStore :: a -> IO (s a)
    reset      :: s a -> IO ()

data ResetStoreObj a = ResetStoreObj {
      store_ :: StoreObj a -- This could be (Store s), imagine the possibilities.
    , reset_ :: IO ()
    }

newResetStore :: a -> IO (ResetStoreObj a)
newResetStore x = do
    s <- store x

    let resetImp = save s x

    return $ ResetStoreObj s resetImp

instance Store ResetStoreObj where
    store     = newResetStore
    load  r   = load (store_ r)
    save  r x = save (store_ r) x

instance ResetStore ResetStoreObj where
    resetStore = newResetStore
    reset      = reset_

main :: IO ()
main = do
    s :: ResetStoreObj Int <- store 0
    save s 3
    x1 <- load s
    print x1
    reset s
    x2 <- load s
    print x2
