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
