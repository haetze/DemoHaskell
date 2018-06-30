-- Unicode.hs
-- Unicode example in Haskell Code
{-# LANGUAGE UnicodeSyntax #-}

module Unicode where
-- Types are here greek chars
reverse':: [α]  -> [α]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]
