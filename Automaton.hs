{-# LANGUAGE MultiParamTypeClasses#-}

module Automaton where

data State = Esum | Osum
  deriving(Show, Read, Eq, Enum)

data Count = ZeroA | OneA | TwoA
  deriving(Show, Read, Eq, Enum)


data DFA q sigma lambda = DFA { currentStateDFA:: q
                              , deltaDFA       :: sigma -> (DFA q sigma lambda)
                              , betaDFA        :: lambda
                              }


unfoldDFA:: DFA q sigma lambda -> [sigma] -> lambda
unfoldDFA automaton [] = betaDFA automaton
unfoldDFA automaton (x:xs) = unfoldDFA (deltaDFA automaton x) xs




esumAutomaton = DFA { currentStateDFA = Esum, deltaDFA = f, betaDFA = True}
  where
    f  x | even x = esumAutomaton
         | True   = DFA { currentStateDFA = Osum, deltaDFA = f', betaDFA = False}
    f' x | odd  x = esumAutomaton
         | True   = DFA { currentStateDFA = Osum, deltaDFA = f', betaDFA = False}


data EpsilonNFAState q sigma lambda = EpsilonNFAState { currentStateENFA:: q
                                                      , epsilonDeltaENFA::          [EpsilonNFAState q sigma lambda]
                                                      , deltaENFA       :: sigma -> [EpsilonNFAState q sigma lambda]
                                                      , betaENFA        :: lambda
                                                      }


unfoldEpsilonNFA:: EpsilonNFAState q sigma lambda -> [sigma] -> [lambda]
unfoldEpsilonNFA automaton []     = [betaENFA automaton]
unfoldEpsilonNFA automaton (x:xs) =  concatMap (flip unfoldEpsilonNFA (x:xs)) (epsilonDeltaENFA automaton)
                                  ++ concatMap (flip unfoldEpsilonNFA xs) (deltaENFA automaton x)

esumAutomaton' = EpsilonNFAState { currentStateENFA = Esum, deltaENFA = f, betaENFA = True, epsilonDeltaENFA = []}
  where
    f  x | even x = [esumAutomaton']
         | True   = [EpsilonNFAState { currentStateENFA = Osum, deltaENFA = f', betaENFA = False, epsilonDeltaENFA = []}]
    f' x | odd  x = [esumAutomaton']
         | True   = [EpsilonNFAState { currentStateENFA = Osum, deltaENFA = f', betaENFA = False, epsilonDeltaENFA = []}]


start = EpsilonNFAState {currentStateENFA = ZeroA, deltaENFA = f, betaENFA = True , epsilonDeltaENFA = [a0Automaton, a0Automaton']}
  where
    f _ = []

a0Automaton' = EpsilonNFAState {currentStateENFA = ZeroA, deltaENFA = f, betaENFA = True , epsilonDeltaENFA = []}
  where
    f 'a' = a1Automaton' : []
    f  _  = a0Automaton' : []

a1Automaton' = EpsilonNFAState {currentStateENFA = OneA,  deltaENFA = f, betaENFA = False , epsilonDeltaENFA = []}
  where
    f 'a' = a0Automaton' : []
    f  _  = a1Automaton' : []


a0Automaton = EpsilonNFAState {currentStateENFA = ZeroA, deltaENFA = f, betaENFA = True , epsilonDeltaENFA = []}
  where
    f 'a' = a1Automaton : []
    f  _  = a0Automaton : []
 
a1Automaton = EpsilonNFAState {currentStateENFA = OneA,  deltaENFA = f, betaENFA = False, epsilonDeltaENFA = []}
  where
    f 'a' = a2Automaton : []
    f  _  = a1Automaton : []

a2Automaton = EpsilonNFAState {currentStateENFA = TwoA,  deltaENFA = f, betaENFA = False, epsilonDeltaENFA = []}
  where
    f 'a' = a0Automaton : []
    f  _  = a2Automaton : []
