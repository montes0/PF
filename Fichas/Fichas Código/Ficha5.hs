module Ficha5 where
import System.Win32 (COORD(xPos), lANG_AFRIKAANS)

any :: (a -> Bool) -> [a] -> Bool
any p [] = True
any p (x:xs) | (p x == False) = any p xs
             | otherwise = True

zipWith :: (a->b->c) -> [a] -> [b] -> [c]
zipWith p (x:xs) (y:ys) = (p x y) : zipWith p xs ys


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p (x:xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
    | p x = dropWhile' x
    | otherwise = []

--span :: (a-> Bool) -> [a] -> ([a],[a])

deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' p x [] = []
deleteBy' p x (y:ys)
 | p x y = ys
 | otherwise = y : deleteBy' p x ys

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f (x:xs) = insert x (sortOn' f xs)
    where insert x [] = [x]
          insert x (y:ys)
           | f x < f y = x : y : ys
           | otherwise = y : insert x ys

--------------------------------------------------------------------

type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau x [] = []
selgrau x [(a,b):xs] | x==b = (a,b) : selgrau x xs
                     | otherwise = selgrau x xs

selgrau' :: Int -> Polinomio -> Polinomio
selgrau' g p = (\(c,t) -> t == g) p

conta :: Int -> Polinomio -> Int
conta g p = length(selgrau g p)

grau :: Polinomio -> Int
grau p = maximum (map snd p)

deriv :: Polinomio -> Polinomio
deriv p = filter (\(c,e) -> c/=0) (map (\(c,e) -> (c*e,e-1))p)

calcula :: Float -> Polinomio -> Float
calcula x [] = 0
calcula x ((a,b):xs) = (a*(x^b)) + calcula x xs

simp :: Polinomio -> Polinomio
simp [] = []
simp l = filter (\(a,b) -> a /= 0) l

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (a,b) l = map (\(c,d) -> (a*c, b+d)) l

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena l = sortOn snd l

--------------------------------------------------------------------

type Mat a = [[a]]

dimOK :: Mat a -> Bool
dimOK [l] = True
dimOK (l1:l2:m) = (length l1) == (length l2) && dimOK (l2:m)

dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat l = (length $ head l,length l)

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat m1 m2 
 | dimMat m1 /= dimMat m2 = error "As matrizes devem ter a mesma dimensao"   
 | otherwise  = (zipWith (+) (head m1) (head m2)) : addMat (tail m1) (tail m2)

transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose m = (map head m) : transpose (map tail m)

