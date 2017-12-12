{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes#-}

module Automaton where

class DFA automaton state sigma lambda  where
  deltaDFA         :: automaton state sigma lambda      -> sigma   -> automaton state sigma lambda
  betaDFA          :: automaton state sigma lambda                 -> lambda
  startingStateDFA :: automaton state sigma lambda
  unfoldDFA        :: automaton state sigma lambda      -> [sigma] -> lambda
  unfoldDFA a []     = betaDFA a
  unfoldDFA a (x:xs) = unfoldDFA (deltaDFA a x) xs


data State = Esum | Osum
  deriving(Show, Read, Eq, Enum)

data Count = ZeroA | OneA | TwoA
  deriving(Show, Read, Eq, Enum)

data Automaton state sigma lambda = Automaton state [sigma] lambda deriving(Show, Read)

instance DFA Automaton State Int Bool where
  startingStateDFA = Automaton Esum [] True 
  deltaDFA (Automaton Esum xs _) x
    | even x = Automaton Esum (x:xs) True
    | odd  x = Automaton Osum (x:xs) False
  deltaDFA (Automaton Osum xs _) x
    | even x = Automaton Osum (x:xs) False
    | odd  x = Automaton Esum (x:xs) True
  betaDFA (Automaton _ _ y) = y


instance DFA Automaton Count Char Bool where
  startingStateDFA = Automaton ZeroA [] True
  deltaDFA (Automaton ZeroA xs _) 'a' = Automaton OneA  ('a':xs) False
  deltaDFA (Automaton OneA  xs _) 'a' = Automaton TwoA  ('a':xs) False
  deltaDFA (Automaton TwoA  xs _) 'a' = Automaton ZeroA ('a':xs) True
  deltaDFA (Automaton state xs b)  x  = Automaton state (x  :xs)   b
  betaDFA (Automaton    _   _  b)     = b

class NFA automaton state sigma lambda  where
  delta         :: automaton state sigma lambda      -> sigma   -> [automaton state sigma lambda]
  beta          :: automaton state sigma lambda                 -> lambda
  startingState :: automaton state sigma lambda
  unfold        :: automaton state sigma lambda      -> [sigma] -> [lambda]
  unfold a []     = [beta a]
  unfold a (x:xs) = concatMap (flip unfold xs) (delta a x) 



instance NFA Automaton State Int Bool where
  startingState = Automaton Esum [] True
  delta (Automaton Esum xs _) x
    | even x = [Automaton Esum (x:xs) True ]
    | odd  x = [Automaton Osum (x:xs) False]
  delta (Automaton Osum xs _) x
    | even x = [Automaton Osum (x:xs) False]
    | odd  x = [Automaton Esum (x:xs) True ]
  beta (Automaton _ _ y) = y
    
