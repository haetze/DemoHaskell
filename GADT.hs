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

data Vec a :: Nat -> * where
  Nil  :: Vec a Z
  Cons :: a -> Vec a n -> Vec a (S n)

deriving instance (Show a) => Show (Vec a n)
  
tail:: Vec a (S n) -> Vec a n
tail (Cons _ t) = t


-- f:: * -> Int
-- f _ = undefined
