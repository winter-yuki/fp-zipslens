module Lib where

import Data.List (partition, uncons)
import Data.Word (Word8)

-- Определение fixpoint уровня типов и морфизмы

newtype Fix f = In (f (Fix f))

deriving instance Show (f (Fix f)) => Show (Fix f)
deriving instance Eq (f (Fix f)) => Eq (Fix f)

out :: Fix f -> f (Fix f)
out (In x) = x

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
cata phi (In x) = phi $ fmap (cata phi) x

type Coalgebra f a = a -> f a

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana psi x = In $ fmap (ana psi) (psi x)

hylo :: Functor f => Algebra f a -> Coalgebra f b -> (b -> a)
hylo phi psi = cata phi . ana psi

-- 1. Дан рекурсивный тип данных. Определите его же через Fix, объявите
-- нерекурсивное определение функтором и реализуйте для него заданные функции.

--                    Empty'   == 0
--              One'  Empty'   == 1
--       Zero'  (One' Empty')  == 2
--       One'   (One' Empty')  == 3
-- Zero' (Zero' (One' Empty')) == 4
data Bin' = Empty' | Zero' Bin | One' Bin

-- Замените на своё определение.
-- four = In $ Zero $ In $ Zero $ In $ One $ In Empty
data BinF rec = Empty | Zero rec | One rec
  deriving (Eq, Show, Ord)

instance Functor BinF where
  fmap _ Empty = Empty
  fmap f (Zero b) = Zero (f b)
  fmap f (One  b) = One  (f b)

type Bin = Fix BinF

-- Подставьте свои конструкторы
emptyCtor = Empty
zeroCtor = Zero
oneCtor = One

phiBin :: Algebra BinF Int -- BinF Int -> Int
phiBin Empty = 0
phiBin (Zero n) = n * 2
phiBin (One  n) = n * 2 + 1

bin2int :: Bin -> Int
bin2int = cata phiBin

psiBin :: Coalgebra BinF Int -- Int -> BinF Int
psiBin 0 = Empty
psiBin n
  | even n    = Zero (n `div` 2)
  | otherwise = One  (n `div` 2)

int2bin :: Int -> Bin
int2bin = ana psiBin
