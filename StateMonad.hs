-- Created on 08 Jan 2021 by richard.stewing@udo.edu
-- Copyright Richard Stewing,08 Jan 2021
-- Licensed under GPLv3,

module StateMonad where

-- Reader-Monad
newtype R a b = R (a -> b)

instance Functor (R a) where
  -- fmap :: (b -> c) -> R a b -> R a c
  fmap f (R g) = R $ f . g

instance Applicative (R a) where
  -- pure :: b -> (R a b)
  pure x = R $ \_ -> x
  -- (<*>) :: R a (b -> c) -> (R a b) -> (R a c)
  (R f) <*> (R g) = R $ \a -> f a (g a)

instance Monad (R a) where
  -- return :: b -> (R a b)
  return x = R $ \_ -> x
  -- (>>=) :: R a b -> (b -> R a c) -> R a c
  (R f) >>= g = R h where h a = let (R h') = g (f a) in h' a 

-- Example for using Reader

fib 0 = 1
fib n = n * fib (n-1)

-- fibsum uses the Reader Monad
-- We don't need to repeat the argument for the calls to fib.
-- Basically we get a constant argument to all functions
fibsum :: (Eq a, Num a) => a -> a
fibsum = do
  a <- fib
  b <- fib
  return $ a + b


-- Writer-Monad
newtype W a b = W (a, b)

instance Functor (W a) where
  -- fmap :: (b -> c) -> W a b -> W a c
  fmap f (W (a,b)) = W (a, f b)

instance (Monoid a) => Applicative (W a) where
  -- pure :: b -> W a b
  pure x = W (mempty, x)
  -- (<*>) :: W a (b -> c) -> W a b -> W a c
  (W (a, f)) <*> (W (a', b)) = W (a `mappend` a', f b)

instance Monoid a => Monad (W a) where
  -- return :: b -> W a b
  return x = W (mempty, x)
  -- (>>=) :: W a b -> (b -> W a c) -> W a c
  (W (a, b)) >>= f = let W (a', c) = f b in W (a `mappend` a', c)

fibMemInput 0 = ([0], 1)
fibMemInput n = let (_, x) = fibMemInput (n-1) in ([n], n*x)


fibinputs a b = do
  x <- fibMemInput a
  y <- fibMemInput b
  return $ x + y

-- Reader + Writer = State-Monad
newtype S a b = S (a -> (b, a))

instance Functor (S a) where
  -- fmap :: (b -> c) -> S a b -> S a c
  fmap f (S g) = S $ (\(b, a) -> (f b, a)) . g

instance Applicative (S a) where
  -- pure :: b -> S a b
  pure x = S $ \a -> (x, a)
  -- (<*>) :: S a (b -> c) -> S a b -> S a c
  (S f) <*> (S g) = S h
    where
      h a = let (f', a') = f a
                (b, a'') = g a'
            in (f' b, a'')

instance Monad (S a) where
  -- return :: b -> S a b
  return x = S $ \a -> (x, a)
  -- (>>=) :: S a b -> (b -> S a c) -> S a c
  (S f) >>= g = S h
    where
      h a = let (b, a') = f a
                S g' = g b in g' a'


fibWithDec 0 = (1, 0)
fibWithDec n = let (x, _) = fibWithDec (n-1) in (x*n, n-1)

fibWithDecTwice :: (Eq b, Num b) => S b b
fibWithDecTwice = do
  a <- S fibWithDec
  b <- S fibWithDec
  return $ a + b


runS :: S a b -> a -> (b, a)
runS (S f) a = f a
