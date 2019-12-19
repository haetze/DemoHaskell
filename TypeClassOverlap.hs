-- Created on 19 Dec 2019 by richard.stewing@udo.edu
{-# LANGUAGE FlexibleInstances #-}

module TypeClassOverlap where

data L a = Nil | Cons a (L a)

class C a where
  c :: a -> String


instance C  (L Int) where
  c _ = "List Int"


instance C  (L Char) where
  c _ = "String"


instance C (L a) where
  c _ = "List a"


x :: L Int
x = Nil

--Fails because of overlapping instances
--test = c x





