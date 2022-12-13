module Lib where

import Data.List (partition, uncons)
import Data.Word (Word8)

-- Определение fixpoint уровня типов и морфизмы.

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

-- Пара стандартных типов из лекции.

data ListF x xs = Nil | Cons x xs
  deriving (Eq, Show, Functor)

type List a = Fix (ListF a)

-- | [3, 4, 5]
listExample :: List Int
listExample = In $ Cons 3 $ In $ Cons 4 $ In $ Cons 5 $ In Nil

data NatF n = Z | S n
  deriving (Show, Eq, Functor)

type Nat = Fix NatF

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

phiExpr :: ExprF Int -> Int
phiExpr = undefined

evalExpr :: Expr -> Int
evalExpr = cata phiExpr

phiExprShow :: ExprF String -> String
phiExprShow = undefined

showExpr :: Expr -> String
showExpr = cata phiExprShow

-- 3. В этом задании будем компилировать выражения из предыдущего
-- задания в байткод и исполнять их на стековой машине.

data Instruction = NumInstr Int | AddInstr | MultInstr
  deriving stock (Eq)

instance Show Instruction where
  show = \case
    NumInstr n -> show n
    AddInstr -> "+"
    MultInstr -> "*"

type ByteCode = [Instruction]

showByteCode :: ByteCode -> String
showByteCode = unwords . map show

-- У байткода есть свойство выгодно отличающее его от других кодов:
-- конкатенация двух валидных программ в байткоде - валидная программа.
-- Это сильно упрощает кодогенерацию и её отладку, поэтому байткод часто
-- используют как промежуточное представление перед компиляцией в машинный код.
-- Мы будем генерировать отдельно байткод для каждой из веток выражения, а потом
-- его конкатенировать. Чтобы это работало эффективнее, будем использовать идиому
-- https://wiki.haskell.org/Difference_list.
phiComp :: Algebra ExprF (ByteCode -> ByteCode)
phiComp = undefined

compile :: Expr -> ByteCode
compile e = cata phiComp e []

-- Определим стек для дальнейшей реализации стековой машины.

type Stack = [Int]

push :: Int -> Stack -> Stack
push a as = a : as

add :: Stack -> Stack
add  (a : b : cs) = (b + a) : cs

mult :: Stack -> Stack
mult (a : b : cs) = (b * a) : cs

-- Стековая машина исполняет байткод и возвращает конечное состояние стека.
evalSM :: ByteCode -> Stack
evalSM = undefined
