{-# LANGUAGE RankNTypes #-}

module Lifting.RNT where

newtype Cont a = Cont {unCont :: forall r. (a -> r) -> r}

-- Ex 6.41
