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


