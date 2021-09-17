{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Vector where

import Data.Kind (Type)
import Data.Singletons (SingI, sing)

import Nat

data Vector (n :: Nat) (a :: Type) where
    Nil :: Vector Zero a
    Cons :: a -> Vector n a -> Vector (Succ n) a

--vecFromList ::  -> [a] -> Vector n a
--vecFromList = undefined

instance Foldable (Vector n) where
    foldr f z Nil = z
    foldr f z (Cons x xs) = f x (foldr f z xs)

instance Functor (Vector n) where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Applicative (Vector n) where
    pure a = undefined--vecOf @(Proxy n) a
    Nil <*> Nil = Nil
    (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)

vecOf :: a -> Vector n a
vecOf = undefined

zipVec :: Vector n a -> Vector n b -> Vector n (a, b)
zipVec Nil Nil = Nil
zipVec (Cons x xs) (Cons y ys) = Cons (x, y) (zipVec xs ys) 
