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


-- Линзы.
