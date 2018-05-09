{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module GADT where

import GHC.Types
import GHC.TypeLits


data Vec a :: Nat -> * where
  Nil  :: Vec a 0
  Cons :: a -> Vec a n -> Vec a (n :+ 1)

data Limit :: Nat -> * where
  LimitZero :: forall (n::Nat) . Limit (n :+ 1)
  LimitN    :: forall (n::Nat) . Limit n -> Limit (n :+ 1)


hd :: forall a (n::Nat) . Vec a (n :+ 1) -> a
hd (Cons h _) = h

tl :: forall a (n::Nat) . Vec a (n :+ 1) -> Vec a n
tl (Cons _ t) = t


isNil:: forall a (n::Nat) . Vec a n -> Bool
isNil Nil = True
isNil  _  = False

instance Eq a => Eq (Vec a n) where
  (==) = eq

eq :: forall a (m::Nat) (n::Nat) . Eq a => Vec a m -> Vec a n -> Bool
eq (Cons h t) (Cons h' t') = h == h' && t `eq` t'
eq Nil Nil                 = True
eq _ _                     = False 

ex1 = Cons 10 Nil `eq` Nil

--Doesn't compile
--ex1' = Cons 10 Nil == Nil

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance 0     :+ m = m
type instance (succ n) :+ m = succ (n :+ m)

{-
append :: forall a (n::Nat) (m::Nat) . Vec a n -> Vec a m -> Vec a (n :+ m)
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ append xs ys
-}

len :: forall a (n::Nat) . Vec a n -> Int
len Nil = 0
len (Cons _ t) = 1 + len t

(!!!):: forall a (n::Nat) . Limit n -> Vec a n -> a
(!!!) LimitZero (Cons h _) = h
(!!!) (LimitN n) (Cons _ t) = undefined


