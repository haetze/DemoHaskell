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


data EpsilonNFA q sigma lambda = EpsilonNFA { currentStateENFA:: q
                                            , epsilonDeltaENFA::          [EpsilonNFA q sigma lambda]
                                            , deltaENFA       :: sigma -> [EpsilonNFA q sigma lambda]
                                            , betaENFA        :: lambda
                                            }


unfoldEpsilonNFA:: EpsilonNFA q sigma lambda -> [sigma] -> [lambda]
unfoldEpsilonNFA automaton []     = [betaENFA automaton]
unfoldEpsilonNFA automaton (x:xs) =  concatMap (flip unfoldEpsilonNFA (x:xs)) (epsilonDeltaENFA automaton)
                                  ++ concatMap (flip unfoldEpsilonNFA xs) (deltaENFA automaton x)

esumAutomaton' = EpsilonNFA { currentStateENFA = Esum, deltaENFA = f, betaENFA = True, epsilonDeltaENFA = []}
  where
    f  x | even x = [esumAutomaton']
         | True   = [EpsilonNFA { currentStateENFA = Osum, deltaENFA = f', betaENFA = False, epsilonDeltaENFA = []}]
    f' x | odd  x = [esumAutomaton']
         | True   = [EpsilonNFA { currentStateENFA = Osum, deltaENFA = f', betaENFA = False, epsilonDeltaENFA = []}]
