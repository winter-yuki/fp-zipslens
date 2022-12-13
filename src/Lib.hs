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
data BinF rec = BinF
  deriving (Eq, Show, Ord)

instance Functor BinF where
  fmap = undefined

type Bin = Fix BinF

-- Подставьте свои конструкторы
emptyCtor = undefined -- Empty
zeroCtor = undefined -- Zero
oneCtor = undefined -- One

phiBin :: Algebra BinF Int -- BinF Int -> Int
phiBin = undefined

bin2int :: Bin -> Int
bin2int = cata phiBin

psiBin :: Coalgebra BinF Int -- Int -> BinF Int
psiBin = undefined

int2bin :: Int -> Bin
int2bin = ana psiBin

-- 2. Определите соответствующий нерекурсивный тип и заданные операции.

data Expr' = Num' Int | Add' Expr Expr | Mult' Expr Expr

-- Замените на свое определение.
data ExprF rec = ExprF
  deriving (Eq, Show, Ord)

instance Functor ExprF where
  fmap = undefined

type Expr = Fix ExprF

numCtor = undefined -- Num
addCtor = undefined -- Add
multCtor = undefined -- Mult

-- Выражения для тестов
en     = In . numCtor
e3     = en 3
ep35   = In (addCtor e3 (en 5))
emp357 = In (multCtor ep35 (en 7))
em7p35 = In (multCtor (en 7) ep35)

phiE :: ExprF Int -> Int
phiE = undefined

eval :: Expr -> Int
eval = cata phiE

phiEShow :: ExprF String -> String
phiEShow = undefined

showExpr :: Expr -> String
showExpr = cata phiEShow
