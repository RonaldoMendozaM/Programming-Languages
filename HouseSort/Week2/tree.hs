module Tree where

import Control.Applicative
import Data.Char (GeneralCategory(NotAssigned))
import Control.Arrow (ArrowChoice(right))

data BinTree a = Empty
              | Node a (BinTree a) (BinTree a)
              deriving (Show, Eq)

instance Functor BinTree 
  where
    fmap :: (a -> b) -> BinTree a -> BinTree b
    fmap _ Empty = Empty
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Applicative BinTree 
  where
    ---pure :: a -> BinTree a
    pure a = Node a Empty Empty
    --(<*>):: BinTree (a -> b) -> BinTree a -> BinTree b
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    (Node g gl gr) <*> (Node a l r) = Node (g a) (gl <*> l) (gr <*> r)

incrementar :: Num a => BinTree a -> BinTree a
incrementar = fmap (+1)

converts :: BinTree String -> BinTree Int
converts = fmap length


--iterator :: Num a => BinTree a -> a -> BinTree a
--iterator (Node _ Empty right) = iterator right
--iterator (Node _ left Empty) = iterator left

myList :: [a] -> BinTree a
myList [] = Empty

--Define a function that merges two trees element-wise using a binary function, assuming both trees have the same structure



mergues ::(a -> b -> c) -> BinTree a -> BinTree b -> BinTree c
mergues _ Empty _ = Empty
mergues _ _ Empty = Empty
mergues f (Node a l r ) (Node b l' r') = Node (f a b) (mergues f l l') (mergues f r r')


merguesTree :: BinTree a -> BinTree a -> BinTree a
merguesTree treeA Empty = treeA
merguesTree Empty treeB = treeB
merguesTree (Node a l r) treeB = merguesTree r (merguesTree l (insert a treeB))

insert :: a -> BinTree a -> BinTree a
insert x Empty = Node x Empty Empty
insert x (Node a l r) = Node a (insert x l) r




tree1 = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)
tree2 = Node 4 (Node 5 Empty Empty) (Node 6 Empty Empty)



main :: IO ()
main = do

  print $ mergues (*) (Node 1 (Node 2 Empty Empty) (Node 2 Empty Empty)) (Node 1 (Node 3 Empty Empty) (Node 4 Empty Empty))
  print $ merguesTree tree1 tree2



--Alternative Functors
-----------------------

addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes (Just a) (Just b) = Just (a + b)


--instance Alternative MyList where
--  empty :: MyList a
--  empty = Empty
--  (<|>) :: MyList a -> MyList a -> MyList a
--  Empty <|> xs = xs
--  xs <|> Empty = xs
--  (Cons x xs) <|> ys = Cons x (xs <|> ys)
  
example :: Maybe Int
example = Just 1 <|> Nothing