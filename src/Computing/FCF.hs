{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Computing.FCF where

import Data.Kind (Constraint, Type)
import Prelude hiding (fst)

data Fst a b = Fst (a, b)

class Eval l t | l -> t where
    eval :: l -> t

instance Eval (Fst a b) a where
    eval (Fst (a, _)) = a

-- Exercise 10.1-i)
data ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe [a]) (Maybe a) where
    eval (ListToMaybe []) = Nothing

--    eval (ListToMaybe (x : _)) = Just x

type Exp a = a -> Type
type family Eval2 (e :: Exp a) :: a

data Snd :: (a, b) -> Exp a
type instance Eval2 (Snd '(a, b)) = b

data FromMaybe :: a -> Maybe a -> Exp a
type instance Eval2 (FromMaybe _1 ('Just a)) = a
type instance Eval2 (FromMaybe a 'Nothing) = a

-- Exercise 10.2-i)
data ListToMaybe2 :: [a] -> Exp (Maybe a)
type instance Eval2 (ListToMaybe2 '[]) = 'Nothing
type instance Eval2 (ListToMaybe2 (x ': xs)) = ('Just x)

data MapList :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval2 (MapList f '[]) = '[]
type instance
    Eval2 (MapList f (a ': as)) =
        Eval2 (f a) ': Eval2 (MapList f as)
