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
    toMat = id

toMat :: (SingI m, SingI n) => [[a]] -> Matrix m n a
toMat xs = Matrix $ toVec $ toVec <$> xs

(<@>) :: (Num a, IsMat x m n a, IsMat y n o a) => x -> y -> Matrix m o a
(<@>) = undefined
