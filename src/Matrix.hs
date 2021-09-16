{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, GADTs #-}

module Matrix where

import Data.Kind (Type)
import GHC.TypeLits (Nat)

import Vector

type Matrix (m :: Nat) (n :: Nat) a = Vector m (Vector n a)
