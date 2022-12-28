module Lib where


-- Зипперы. Реализуйте необходимые функции для зиппера дерева. В случае
-- некорректного использования зиппера нужно бросать runtime исключение.
-- (Для простоты)

data Tree a = Leaf | Node (Tree a) a (Tree a)

type TreeZ a = (a, TreeC a)

data Dir = L | R deriving (Eq, Show)

type TreeC a =
  ( Tree a -- Left subtree of the hole
  , Tree a -- Right subtree of the hole
  , -- Path from root to the focus
    [( Bool   -- Direction to go next
     , a      -- Value on the path from root
     , Tree a -- Subtree from the opposite direction
    )]
  )

mkTreeZ :: Tree a -> TreeZ a
mkTreeZ = undefined

leftTreeZ :: TreeZ a -> TreeZ a
leftTreeZ = undefined

rightTreeZ :: TreeZ a -> TreeZ a
rightTreeZ = undefined

upTreeZ :: TreeZ a -> TreeZ a
upTreeZ = undefined

unTreeZ :: TreeZ a -> Tree a
unTreeZ = undefined

updateTreeZ :: a -> TreeZ a -> TreeZ a
updateTreeZ = undefined

--   2
--  / \
-- 1   4
--    / \
--    3 5
testTree = Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 (Node Leaf 5 Leaf))


-- Линзы Ван Лаарховена. На лекции вы строили линзы как функцию, которая
-- для любого функтора f превращает вложение a -> f a во вложение s -> f s
-- Так и запишем

type Lens'' s a = forall f. Functor f => (a -> f a) -> (s -> f s)

-- Геттер и сеттер упаковываются следующим образом:
-- forall f. Functor f => (s -> a) -> (s -> a -> s) -> (a -> f a) -> s -> f s
lens' :: (s -> a) -> (s -> a -> s) -> Lens'' s a
lens' get set = \ret s -> fmap (set s) (ret $ get s)

-- Примеры линз для пары:

_1' :: Lens'' (a, b) a
_1' = lens' fst (\(_, y) v -> (v, y))

_2' :: Lens'' (a, b) b
_2' = lens' snd (\(x, _) v -> (x, v))

-- А распаковываются с помощью нужных функторов

newtype Const a x = Const { getConst :: a } deriving Show

instance Functor (Const c) where
  fmap _ (Const x) = Const x

view' :: Lens'' s a -> s -> a
view' lns s = getConst (lns Const s)

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

over' :: Lens'' s a -> (a -> a) -> s -> s
over' lns f s = runIdentity $ lns (Identity . f) s

set' :: Lens'' s a -> a -> s -> s
set' lns a = over' lns (const a)

-- Заметим, что линзы _1 и _2 удобно использовать не только для пар, но и для
-- кортежей больших размерностей. Для этого их реализуют не как свободные функции
-- а как представителей классов типов. Например

-- Вам предлагается написать представителей этих классов типов для доступа к
-- элементам внутри пар и троек

-- Давайте обобщим предыдущую конструкцию. У нас были линзы, которые не
-- позволяли менять тип структуры при модификации. Более общий вариант линз:

type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

-- Именно с таким определением и работает библиотека lens
-- Предыдущее определение линз называется Lens' и определяется как

type Lens' s a = Lens s s a a

-- Тогда предыдущим определениям функций lens, view, over, set можно без
-- проблем написать более общие сигнатуры типов

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = \ret s -> fmap (set s) (ret $ get s)

view :: Lens s t a b -> s -> a
view lns s = getConst (lns Const s)

over :: Lens s t a b -> (a -> b) -> s -> t
over lns f s = runIdentity $ lns (Identity . f) s

set :: Lens s t a b -> b -> s -> t
set lns a = over lns (const a)

-- Наконец, перепишите классы Field1, Field2 и Field3 что бы они подружились
-- с новым определением линз.
