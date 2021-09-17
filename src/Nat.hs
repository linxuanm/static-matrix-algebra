{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies#-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Nat where

data Nat = Zero 
         | Succ Nat
         deriving (Eq, Show)

type family (m :: Nat) :+: (n :: Nat) :: Nat
type instance Zero :+: n = n
type instance (Succ m) :+: n = Succ (m :+: n)
