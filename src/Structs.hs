{-# LANGUAGE FunctionalDependencies #-}
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

class IsMat mat m n a | mat -> m n where
    getMat :: mat -> Matrix m n a
