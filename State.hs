{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module State
       where

class State s e where
  step:: s -> e -> s

  
data DataState a = DataState a a |
                   Done
                   deriving(Show, Read)

instance State (DataState String) Char where
  step (DataState (c:cs) (x:[]))  n | n == x = Done
  step (DataState (c:cs) (x:xs)) n | n == x = DataState (c:cs) xs
                                   | n == c = DataState (c:cs) cs
                                   | otherwise = DataState (c:cs) (c:cs)
  step Done _ = Done 

                           


  


