{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies#-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Nat where

import Data.Kind (Type)
import Data.Singletons (sing, Sing, SingI)

data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

instance Num Nat where
    Zero + n = n
    (Succ m) + n = Succ $ m + n
    m - Zero = m
    Zero - n = error "negative natural number"
    (Succ m) - (Succ n) = m - n
    Zero * _ = Zero
    _ * Zero = Zero
    (Succ m) * n = m * n + n
    abs = id
    signum Zero = Zero
    signum (Succ _) = Succ Zero
    fromInteger x
        | x < 0 = error "negative natural number"
        | x == 0 = Zero
        | otherwise = Succ $ fromInteger $ x - 1

data SNat :: Nat -> Type where
    SZero :: SNat Zero
    SSucc :: SNat n -> SNat (Succ n)

type instance Sing = SNat

instance SingI Zero where
    sing = SZero

instance SingI n => SingI (Succ n) where
    sing = SSucc sing

type family (m :: Nat) :+: (n :: Nat) :: Nat
type instance Zero :+: n = n
type instance (Succ m) :+: n = Succ (m :+: n)

type family (m :: Nat) :-: (n :: Nat) :: Nat
type instance Zero :-: _ = Zero
type instance m :-: Zero = m
type instance (Succ m) :-: (Succ n) = m :-: n

type family (m :: Nat) :<: (n :: Nat) :: Bool
type instance _ :<: Zero = False
type instance Zero :<: (Succ _) = True
type instance (Succ m) :<: (Succ n) = m :<: n
