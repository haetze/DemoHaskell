{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module GADT where

import Prelude hiding (tail, head)

data Nat = Z | S Nat

infixl 6 :+
infixl 6 :*

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z     :+ m = m
type instance (S n) :+ m = S (n :+ m)

type family (n::Nat) :* (m::Nat) :: Nat
type instance Z     :* m = Z
type instance (S n) :* m = m :* (n :* m)

data Vec a :: Nat -> * where
  Nil  :: Vec a Z
  Cons :: a -> Vec a n -> Vec a (S n)

deriving instance (Show a) => Show (Vec a n)

  
tail:: Vec a (S n) -> Vec a n
tail (Cons _ t) = t

head:: Vec a (S n) -> a
head (Cons a _) = a

append:: Vec a n -> Vec a m -> Vec a (n :+ m)
append Nil u = u
append (Cons a v) u = Cons a $ append v u

-- repeate:: a -> n -> Vec a n
-- repeate _ Z = Nil
-- repeate a (S n) = Cons a (repeaze a n)



