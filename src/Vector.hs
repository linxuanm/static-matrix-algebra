{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Vector where

import Data.Kind (Type)

import Nat

data Vector (n :: Nat) (a :: Type) where
    Nil :: Vector Zero a
    Cons :: a -> Vector n a -> Vector (Succ n) a

--vecFromList :: (KnownNat n) => [a] -> Vector n a
--vecFromList = undefined

instance Foldable (Vector n) where
    foldr f z Nil = z
    foldr f z (Cons x xs) = f x (foldr f z xs)

instance Functor (Vector n) where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (f <$> xs)
