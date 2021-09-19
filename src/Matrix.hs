{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Matrix where

import Data.Singletons (SingI)

import Nat
import Vector
import Structs

--toMat :: (SingI m, SingI n) => [[a]] -> Matrix m n a
--toMat xs = toVec $ toVec <$> xs