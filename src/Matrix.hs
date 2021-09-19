{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Matrix where

import Data.Singletons (SingI)

import Nat
import Vector
import Structs

instance IsMat (Matrix m n a) m n a where
    getMat = id

instance Show a => Show (Matrix m n a) where
    show (Matrix x) = show x

toMat :: (SingI m, SingI n) => [[a]] -> Matrix m n a
toMat xs = Matrix $ toVec $ toVec <$> xs

row :: (i :<: m ~ True) => SNat i -> Matrix m n a -> Vector n a
row = undefined

col :: (i :<: n ~ True) => SNat i -> Matrix m n a -> Vector m a
col = undefined

(<@>) :: (Num a, IsMat x m n a, IsMat y n o a) => x -> y -> Matrix m o a
(<@>) = undefined
