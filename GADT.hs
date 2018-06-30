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
  Cons :: a -> Vec a n -> Vec a (n + 1)

  
hd :: forall a (n::Nat) . Vec a (n + 1) -> a
hd (Cons h _) = h

-- ? Why doesnt this work?
-- tl :: forall a (n::Nat) . Vec a (n + 1) -> Vec a n
-- tl (Cons _ t) = t


isNil:: Vec a n -> Bool
isNil Nil = True
isNil  _  = False

-- eq:: forall a (n::Nat). Vec a n -> Vec a n -> Bool
-- eq (Cons h t) (Cons h' t') = h == h' && t `eq` t'
-- eq Nil        Nil          = True

