module Ficha7 where
import Data.Sequence (Seq(Empty))

data ExpInt = Const Int
    | Simetrico ExpInt
    | Mais ExpInt ExpInt
    | Menos ExpInt ExpInt
    | Mult ExpInt ExpInt

calcula :: ExpInt -> Int
calcula (Const x)   = x
calcula (Simetrico x) = (-1) * calcula x
calcula (Mais x y)  = (calcula x) + (calcula y)
calcula (Menos x y) = (calcula x) - (calcula y)
calcula (Mult x y)  = (calcula x) * (calcula y)

infixa :: ExpInt -> String 
infixa (Const x) = show x
infixa (Simetrico x) = "(-(" ++ infixa x ++ ")"
infixa (Mais x y) = "(" ++ infixa x ++ " + " ++ infixa y ++ ")"
infixa (Menos x y) = "(" ++ infixa x ++ " - " ++ infixa y ++ ")"
infixa (Mult x y) = "(" ++ infixa x ++ " * " ++ infixa y ++ ")"

posfixa :: ExpInt -> String
posfixa (Const x) = " " ++ show x
posfixa (Simetrico x) = posfixa x ++ " *(-1)"
posfixa (Mais x y) = posfixa x ++ posfixa y ++ " +"
posfixa (Menos x y) = posfixa x ++ posfixa y ++ " -"
posfixa (Mult x y) = posfixa x ++ posfixa y ++ " *"

--------------------------------------------------------------------

data RTree a = R a [RTree a]

soma :: Num a => RTree a -> a
soma (R x lrt) = x + sum (map (soma) lrt)

altura :: RTree a -> Int
ãltura (R x [])  = 1
altura (R x lrt) = 1 + maximum (map (altura) lrt)

prune :: Int -> RTree a -> RTree a
prune 0 (R x _)  = R x []
prune _ (R x []) = (R x [])
prune n (R x lrt) = R x (map (prune (n-1)) lrt)

--Rever
mirror :: RTree a -> RTree a
mirror (R x lrt) = R x (reverse (map (mirror) lrt))

--Rever, ainda com algumas dúvidas
postorder :: RTree a -> [a]
postorder (R x lrt) = (concat (map (postorder) lrt)) ++ [x]

--------------------------------------------------------------------

data BTree a = Empty | Node a (BTree a) (BTree a)

data LTree a = Tip a | Fork (LTree a) (LTree a)

ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork e d) = ltSum e + ltSum d

listLT :: LTree a -> [a]
listLT (Tip x) = [x]
listLT (Fork e d) = listLT e ++ listLT d

ltHeight :: LTree a -> Int
ltHeight (Tip x) = 1
ltHeight (Fork e d) = 1 + (max (ltHeight e) (ltHeight d))

mapLT :: (a -> b) -> LTree a -> LTree b
mapLT f (Tip x) = Tip (f x)
mapLT f (Fork e d) = (Fork (mapLT f e) (mapLT f d))

--------------------------------------------------------------------