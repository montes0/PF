module Ficha6 where

data BTree a = Empty
             | Node a (BTree a) (BTree a)
           deriving Show

altura :: BTree a -> Int
altura Empty = 0
altura (Node a e d) = 1 + max(altura(e) altura (d))

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node x e d) = 1 + (contaNodos e) + (contaNodos d)

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ e d) = folhas e + folhas d

prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty
prune _ Empty = Empty
prune x (Node a e d) = (Node a (prune x-1 e) (prune x-1 d))

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node a e d) = [a]
path (x:xs) (Node a e d) 
        | x = a: path xs d
        | otherwise = a : path xs e

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node a e d) = (Node a (mirror d) (mirror e))

zipWithBT :: (a->b->c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node x e1 d1) (Node y e2 d2) = Node (f x y) (zipWithBT f e1 e2) (zipWithBT f d1 d2)

unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (x,y,z) e d) = (Node x a1 a2, Node y b1 b2, Node z c1 c2)
      where (a1,b1,c1) = unzipBT e
            (a2,b2,c2) = unzipBT d

--------------------------------------------------------------------

minimo :: Ord a => BTree a -> a
minimo (Node x Empty _) = x
minimo (Node x e d) = minimo e

-- Caso não tenho ramificação para a esquerda ele remove o nodo anterior
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty d) = d
semMinimo (Node x e d) = Node x (semMinimo e) d

minSmin :: Ord a => BTree a -> (a, BTree a)
minSmin (Node x Empty d) = (x,d)
minSmin (Node x e d) = (minSmin e, Node x (minSmin e) d)

remove :: Ord a => a -> BTree a -> BTree a
remove x Empty = Empty
remove x (Node y e d)
 | y == x = let (m,smd) = minSmin d
            in (Node m e smd)
 | x < y = Node y (remove x e) d
 | x > y = Node y e (remove x d)

--------------------------------------------------------------------

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
| Rep
| Faltou
deriving Show
type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)