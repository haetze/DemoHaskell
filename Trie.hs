-- Created on 21 Jan 2021 by richard.stewing@udo.edu
-- Copyright Richard Stewing,21 Jan 2021
-- Licensed under GPLv3, See toplevel LICENSE file.

module Trie where

data Trie = Leaf Int | Node (Char -> Maybe Trie) Int

letters = ['A'..'Z']++['a'..'z']

insertWord :: String -> Trie -> Trie
insertWord [] (Leaf n) = Leaf (n+1)
insertWord [] (Node f n) = Node f (n+1)
insertWord (x:xs) (Leaf n) = Node (\c -> if c == x 
                                         then Just (insertWord xs (Leaf 0))
                                         else Nothing) n
insertWord (x:xs) (Node f n) =
  case f x of
    Nothing -> Node (\c -> if c == x then Just (insertWord xs (Leaf 0)) else f c) n
    Just t -> Node (\c -> if c == x then Just (insertWord xs t) else f c) n

collectWords :: Trie -> [String]
collectWords (Leaf n) = replicate n ""
collectWords (Node f n) = endingWords ++ ongoing
  where
    endingWords = replicate n ""
    ongoing = [c:w | c <- letters, Just t <- [f c], w <- collectWords t]


testWords = words "hallo hai hi hail man woman wonder hallo"

testInsert = foldr insertWord (Leaf 0) testWords

testCollect = collectWords testInsert

collectWithCount :: Trie -> [(String, Int)]
collectWithCount (Leaf n) = [("", n)]
collectWithCount (Node f n) = endingWords ++ ongoing
  where
    endingWords = [("", n)]
    ongoing = [(c:w,count) | c <- letters, Just t <- [f c], (w,count) <- collectWithCount t]
