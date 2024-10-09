{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lifting.CAG where

import Data.Kind (Constraint, Type)

data HList (ts :: [Type]) where
    HNil :: HList '[]
    (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :# ts) = 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

showBool :: HList '[_1, Bool, _2] -> String
showBool (_ :# b :# _ :# HNil) = show b

{- Ex 5.3-i
instance Eq (HList '[]) where
    HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
    (a :# as) == (b :# bs) = a == b && as == bs

instance Ord (HList '[]) where
    HNil `compare` HNil = EQ

instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
    (x :# xs) `compare` (y :# ys) = compare x y <> compare xs ys

-- Ex 5.3-ii
instance Show (HList '[]) where
    show HNil = "HNil"

instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
    show (t :# ts) = show t <> " :# " <> show ts
-}
type family AllEq (ts :: [Type]) :: Constraint where
    AllEq '[] = ()
    AllEq (t ': ts) = (Eq t, AllEq ts)

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
    All c '[] = ()
    All c (t ': ts) = (c t, All c ts)

instance (All Eq ts) => Eq (HList ts) where
    HNil == HNil = True
    (a :# as) == (b :# bs) = a == b && as == bs

-- Ex . 5.3-iii
instance (All Eq ts, All Ord ts) => Ord (HList ts) where
    HNil `compare` HNil = EQ
    (a :# as) `compare` (b :# bs) = compare a b <> compare as bs

instance (All Show ts) => Show (HList ts) where
    show HNil = "HNil"
    show (x :# xs) = show x ++ " :# " ++ show xs
