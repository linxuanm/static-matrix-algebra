{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies#-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Nat where

import Data.Kind (Type)
import Data.Singletons (Sing)

data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

--data instance Sing :: Nat -> Type where
--    SZero :: Sing Zero
--    SSucc :: Sing n -> Sing (Succ n)

type family (m :: Nat) :+: (n :: Nat) :: Nat
type instance Zero :+: n = n
type instance (Succ m) :+: n = Succ (m :+: n)
