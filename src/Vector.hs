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
import Debug.Trace

import Nat
import Structs

instance Num a => Num (Vector Zero a) where
    Nil + Nil = Nil
    Nil - Nil = Nil
    Nil * Nil = Nil
    fromInteger _ = Nil
    abs _ = Nil
    signum _ = Nil

instance (Num a, Num (Vector n a)) => Num (Vector (Succ n) a) where
    (Cons x xs) + (Cons y ys) = Cons (x + y) (xs + ys)
    (Cons x xs) - (Cons y ys) = Cons (x - y) (xs - ys)
    (Cons x xs) * (Cons y ys) = Cons (x * y) (xs * ys)
    fromInteger a = Cons (fromInteger a) (fromInteger a)
    abs = fmap abs
    signum = fmap signum

instance Show a => Show (Vector n a) where
    show Nil = "[ ]"
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

toVec :: SingI n => [a] -> Vector n a
toVec = inner sing
    where
        inner :: Sing n -> [a] -> Vector n a
        inner SZero _ = Nil
        inner (SSucc n) (x:xs) = Cons x (inner n xs)
        inner _ [] = error "list shorter than expected vector length"

(<+>) :: Vector m a -> Vector n a -> Vector (m :+: n) a
Nil <+> n = n
(Cons x xs) <+> n = Cons x (xs <+> n)

dotVec :: Num a => Vector n a -> Vector n a -> a
dotVec Nil Nil = 0
dotVec (Cons x xs) (Cons y ys) = x * y + dotVec xs ys

zipVec :: Vector n a -> Vector n b -> Vector n (a, b)
zipVec Nil Nil = Nil
zipVec (Cons x xs) (Cons y ys) = Cons (x, y) (zipVec xs ys) 
