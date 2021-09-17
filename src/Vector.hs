{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Vector where

import Data.Kind (Type)
import Data.Singletons (SingI, Sing, sing)

import Nat

data Vector n a where
    Nil :: Vector Zero a
    Cons :: a -> Vector n a -> Vector (Succ n) a

instance Show a => Show (Vector n a) where
    show a = "[ " ++ foldr1 (\a b -> a ++ "  " ++ b) (show <$> a) ++ " ]"

instance Foldable (Vector n) where
    foldr f z Nil = z
    foldr f z (Cons x xs) = f x (foldr f z xs)

instance Functor (Vector n) where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Applicative (Vector Zero) where
    pure a = Nil
    _ <*> _ = Nil

instance Applicative (Vector n) => Applicative (Vector (Succ n)) where
    pure a = Cons a (pure a)
    (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)

vecOf :: SingI n => a -> Vector n a
vecOf = inner sing
    where
        inner :: Sing n -> a -> Vector n a
        inner SZero a = Nil
        inner (SSucc n) a = Cons a (inner n a)

zipVec :: Vector n a -> Vector n b -> Vector n (a, b)
zipVec Nil Nil = Nil
zipVec (Cons x xs) (Cons y ys) = Cons (x, y) (zipVec xs ys) 
