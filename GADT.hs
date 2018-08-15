{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module GADT where

import Prelude hiding (tail, head)

data Nat = Z | S Nat

instance Num Nat where
  Z + m     = m
  (S n) + m = S (n + m)
  Z * _     = Z
  (S n) * m = m + (n * m)

data Vec a :: Nat -> * where
  Nil  :: Vec a Z
  Cons :: a -> Vec a n -> Vec a (S n)

deriving instance (Show a) => Show (Vec a n)

  
tail:: Vec a (S n) -> Vec a n
tail (Cons _ t) = t

-- append:: Vec a n -> Vec a m -> Vec a (n + m)
-- append Nil u = u
-- append (Cons a v) u = Cons a $ append v u


-- f:: * -> Int
-- f _ = undefined
