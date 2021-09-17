{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Matrix where

import Nat
import Vector

type Matrix (m :: Nat) (n :: Nat) a = Vector m (Vector n a)
