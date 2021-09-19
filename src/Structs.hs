--{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

module Structs where

import Data.Kind (Type)
import Data.Singletons (SingI)

import Nat

data Vector n a where
    Nil :: Vector Zero a
    Cons :: a -> Vector n a -> Vector (Succ n) a

newtype Matrix (m :: Nat) (n :: Nat) a = Matrix (Vector m (Vector n a))

class MatMul mat m n a where
    toMat :: mat n a -> Matrix m n a

instance MatMul (Matrix m) m n a where
    toMat = id

instance MatMul Vector (Succ Zero) n a where
    toMat = Matrix . flip Cons Nil
