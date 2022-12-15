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

-- [3, 4, 5]
listExample :: List Int
listExample = In $ Cons 3 $ In $ Cons 4 $ In $ Cons 5 $ In Nil

data NatF n = Z | S n
  deriving (Show, Eq, Functor)

type Nat = Fix NatF

-- 3
natExample :: Nat
natExample = In $ S $ In $ S $ In $ S $ In Z


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


-- 2. Определите соответствующий нерекурсивный тип и заданные операции.

data Expr' = Num' Int | Add' Expr Expr | Mult' Expr Expr

data ExprF rec = Num Int | Add rec rec | Mult rec rec
  deriving (Eq, Show, Ord)

instance Functor ExprF where
  fmap f (Num n)    = Num n
  fmap f (Add  l r) = Add  (f l) (f r)
  fmap f (Mult l r) = Mult (f l) (f r)

type Expr = Fix ExprF

numCtor = Num
addCtor = Add
multCtor = Mult

-- Выражения для тестов
en     = In . numCtor
e3     = en 3
ep35   = In (addCtor e3 (en 5))
emp357 = In (multCtor ep35 (en 7))
em7p35 = In (multCtor (en 7) ep35)

phiExpr :: ExprF Int -> Int
phiExpr (Num n) = n
phiExpr (Add  l r) = l + r
phiExpr (Mult l r) = l * r

evalExpr :: Expr -> Int
evalExpr = cata phiExpr

phiExprShow :: ExprF String -> String
phiExprShow (Num n)    = show n
phiExprShow (Add  l r) = concat ["(", l, "+", r, ")"]
phiExprShow (Mult l r) = concat ["(", l, "*", r, ")"]

showExpr :: Expr -> String
showExpr = cata phiExprShow


-- 3. Реализуйте map двумя способами.

-- map как катаморфизм поданного на вход списка.
cataMap :: (a -> b) -> List a -> List b
cataMap f = cata $ \case
  Nil -> In Nil
  Cons x xs -> In $ Cons (f x) xs

-- map как анаморфизм генерируемого списка.
anaMap :: (a -> b) -> List a -> List b
anaMap f = ana $ \case
  In Nil -> Nil
  In (Cons x xs) -> Cons (f x) xs


-- 4. Катаморфизмы являются обобщением концепции свёртки списков.
-- Напишем аналог foldr для определённого выше List.

listFoldr :: (a -> b -> b) -> b -> List a -> b
listFoldr _ ini (In Nil) = ini
listFoldr f ini (In (Cons x xs)) = x `f` listFoldr f ini xs

-- Напишите аналог listFoldr через cata.
cataListFoldr :: (a -> b -> b) -> b -> List a -> b
cataListFoldr f ini = cata $ \case
  Nil -> ini
  Cons x xs -> x `f` xs

-- Напишите аналог cata для списков через foldr.
cataList :: (ListF a b -> b) -> List a -> b
cataList f = listFoldr g ini
  where
    ini = f Nil
    g x y = f (Cons x y)


-- 5. Анаморфизмы являются обобщением концепции развёртки списков.
-- Напишем аналог unfoldr для определённого выше List.

listUnfoldr :: (b -> Maybe (a, b)) -> b -> List a
listUnfoldr f b = case f b of
  Nothing -> In Nil
  Just (a, b') -> In (Cons a (listUnfoldr f b'))

-- Напишите аналог unfoldr через ana.
-- Подсказка: убедитесь перед этим, что Maybe (a, b) равномощно ListF a b.
listUnfoldr' :: (b -> Maybe (a, b)) -> b -> List a
listUnfoldr' f = ana $ \x -> case f x of
  Nothing -> Nil
  Just (x, y) -> Cons x y

-- Напишите аналог ana для списков через unfoldr.
anaList :: (b -> ListF a b) -> b -> List a
anaList f = listUnfoldr g
  where
    g b = case f b of
      Nil -> Nothing
      Cons x y -> Just (x, y)


-- 6. В этом задании будем компилировать выражения из второго задания
-- в байткод (здесь - польская нотация) и исполнять его на стековой машине.

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
phiComp = \case
  Num n -> (NumInstr n :)
  Add l r -> (AddInstr :) . l . r
  Mult l r -> (MultInstr :) . l . r

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
evalSM = flip foldr [] $ \case
  NumInstr n -> push n
  AddInstr -> add
  MultInstr -> mult


-- 7. Дано бинарное дерево. Определите для него нерекурсивный функтор
-- и реализуйте функцию суммирования элементов.

data Tree' a = Leaf' | Branch' (Tree' a) a (Tree' a)

-- Замените на свое определение.
data TreeF a rec = Leaf | Branch rec a rec
  deriving stock (Eq, Show)

type Tree a = Fix (TreeF a)

instance Functor (TreeF a) where
  fmap f = \case
    Leaf -> Leaf
    Branch l x r -> Branch (f l) x (f r)

leafCtor = Leaf
branchCtor = Branch

{- Дерево для тестирования:
     5
    / \
   3   6
  / \   \
 2   4   7
-}

iB l x r = In $ branchCtor l x r
iL = In leafCtor

testTree = iB
  (iB
    (iB iL 2 iL)
    3
    (iB iL 4 iL))
  5
  (iB
    iL
    6
    (iB iL 7 iL))

phiTreeSum :: Algebra (TreeF Integer) Integer
phiTreeSum = \case
  Leaf -> 0
  Branch l x r -> l + x + r

treeSum :: Tree Integer -> Integer
treeSum = cata phiTreeSum


-- 8. Воспользуемся деревом из предыдущего задания как удобным промежуточным
-- представлением и реализуем сортировку списка. Сначала список будет разворачиваться
-- в бинарное дерево поиска с помощью анаморфизма. Потом -- дерево будет сворачиваться
-- обратно в список с помощью катаморфизма алгеброй, реализующей in-order обход.
-- Таким образом, решение -- композиция анаморфизма с катаморфизмом -- гилеморфизм,
-- который выражает идею трансформации вещей через удобное промежуточное представление.

phiTreeInorder :: Algebra (TreeF a) [a] -- T a [a] -> [a]
phiTreeInorder Leaf = []
phiTreeInorder (Branch l x r) = l <> [x] <> r

tree2listInorder :: Tree a -> [a]
tree2listInorder = cata phiTreeInorder

psiTreeBST :: Ord a => Coalgebra (TreeF a) [a] -- [a] -> T a [a]
psiTreeBST [] = Leaf
psiTreeBST (x:xs) = let (l, r) = partition (< x) xs in Branch l x r

list2BST :: Ord a => [a] -> Tree a
list2BST = ana psiTreeBST

sort :: Ord a => [a] -> [a]
sort = hylo phiTreeInorder psiTreeBST


-- 9. Вспомним из лямбда-исчисления, как можно получить на единицу меньшее
-- натуральное число, и запишем это через катаморфизмы:

natPred :: Nat -> Nat
natPred = snd . cata alg
  where
    alg :: NatF (Nat, Nat) -> (Nat, Nat)
    alg Z = (In Z, In Z)
    alg (S (cur, _)) = (In (S cur), cur)

-- Далее вспомним оттуда же комбинатор примитивной рекурсии и закодируем его:

natPrimRec
  :: forall a. (Nat -> a -> a)
  -> a
  -> Nat
  -> a
natPrimRec f a = snd . cata alg
  where
    alg :: NatF (Nat, a) -> (Nat, a)
    alg Z = (In Z, a)
    alg (S (prd, a)) = (In (S prd), f prd a)

natPred' = natPrimRec const (In Z)

-- Выразим это для удобства чуть иначе (потом будет ясно, зачем):

natPrimRec'
  :: forall a. (NatF (Nat, a) -> a)
  -> Nat
  -> a
natPrimRec' f = snd . cata alg
  where
    alg :: NatF (Nat, a) -> (Nat, a)
    alg n@Z = (In Z, f n)
    alg n@(S (prd, a)) = (In (S prd), f n)

natPred'' = natPrimRec' $ \case
  Z -> In Z
  S (prd, a) -> prd

-- Вспомним теперь, как через foldr (и просто в лямбда-исчислении) искать хвост списка:

cataTail :: List a -> List a
cataTail = snd . cata alg
  where
    alg :: ListF a (List a, List a) -> (List a, List a)
    alg Nil = (In Nil, In Nil)
    alg (Cons x (cur, _)) = (In (Cons x cur), cur)

-- Обобщим natPrimRec до новой концепции -- параморфизма. Параморфизм работает
-- почти так же, как катаморфизм, но позволяет на каждом шаге получать доступ не
-- только к результатам свертки подструктур, но и к самим частям структуры,
-- свёртка которых привела к таким значениям.

-- `natPrimRec'` выше является параморфизмом для натуральных чисел. Если вместо `f`
-- в типе `para` подставить `NatF`, то их типы сойдутся.

-- Используя `cata`, реализуйте `para` и выразите через него `tail`.

para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para f = snd . cata (\g -> let a = f g in (In $ fmap fst g, a))

paraTail :: List a -> List a
paraTail = para $ \case
  Nil -> In Nil
  Cons _ (t, _) -> t

-- 10. Используя параморфизм, найдите левое поддерево бинарного дерева.

paraLeftmost :: Tree a -> Maybe (Tree a)
paraLeftmost = para $ \case
  Leaf -> Nothing
  Branch (lt, l) a (rt, r) -> Just lt
