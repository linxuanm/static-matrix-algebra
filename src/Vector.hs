{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, GADTs #-}

module Vector where

import Data.Kind (Type)
import GHC.TypeLits (Nat, type (+))

data Vector (n :: Nat) (a :: Type) where
    Nil :: Vector 0 a
    Cons :: Vector n a -> Vector (n + 1) a
