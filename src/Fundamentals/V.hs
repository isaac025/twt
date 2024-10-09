module Fundamentals.V where

-- Exercise 3-i) Which are functors and give their instances?
newtype T1 a = T1 (Int -> a)
newtype T2 a = T2 (a -> Int)
newtype T3 a = T3 (a -> a)
newtype T4 a = T4 ((Int -> a) -> Int)
newtype T5 a = T5 ((a -> Int) -> Int)

-- A: T1, T5
instance Functor T1 where
    fmap f (T1 a) = T1 $ fmap f a

instance Functor T5 where
    fmap f (T5 g) = T5 $ \b -> g $ b . f
