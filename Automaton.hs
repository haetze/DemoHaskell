{-# LANGUAGE MultiParamTypeClasses#-}

module Automaton where

data State = Esum | Osum
  deriving(Show, Read, Eq, Enum)

data Count = ZeroA | OneA | TwoA
  deriving(Show, Read, Eq, Enum)


esumAutomaton = DFA { currentState = Esum, delta = f, beta = True}
  where
    f  x | even x = esumAutomaton
         | True   = DFA { currentState = Osum, delta = f', beta = False}
    f' x | odd  x = esumAutomaton
         | True   = DFA { currentState = Osum, delta = f', beta = False}



data DFA q sigma lambda = DFA {currentState:: q, delta:: sigma -> (DFA q sigma lambda), beta:: lambda}


unfoldDFA:: DFA q sigma lambda -> [sigma] -> lambda
unfoldDFA automaton [] = beta automaton
unfoldDFA automaton (x:xs) = unfoldDFA (delta automaton x) xs
