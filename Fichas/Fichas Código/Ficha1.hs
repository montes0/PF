{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Ficha1 where

perimetro :: Float -> Float
perimetro r = 2 * pi * r

dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

primUlt :: [a] -> (a,a)
primUlt (x:xs) = (x, last xs)

multiplo :: Int -> Int -> Bool
multiplo m n = m `mod` n == 0

truncImpar :: [a] -> [a]
truncImpar l | (length l `mod` 2 == 0) = l
             | otherwise = tail l

max2 :: Int -> Int -> Int
max2 x y | (x>y) = x
         | otherwise = y

max3 :: Int -> Int -> Int 
max3 x y z = max2(max2 x y) z 

--------------------------------------------------------------------

nRaizes :: Double -> Double -> Double -> Int
nRaizes a b c = if delta < 0 then 0 
                else if delta == 0 then 1 
                else 2
                where delta = b*b - 4*a*c

raizes :: Double -> Double -> Double -> [Double]
raizes a b c = 
  case nRaizes a b c of
    0 -> []
    1 -> [-b/(2*a)]
    2 -> [(-b + sqrt(delta))/(2*a), (-b - sqrt(delta))/(2*a)]
  where delta = b^2 - 4*a*c

--------------------------------------------------------------------

type Hora = (Int,Int)

real :: Hora -> Bool
real (x,y) = (elem x [0..23]) && (elem y [0..59])

hora :: Hora -> Hora -> Bool
hora (x,xs) (y,ys) | x > y = True
                   | (x == y) && (xs>ys) = True
                   | otherwise = False

min :: Hora -> Int
min (x,y) = x*60 + y

minHor :: Int -> Hora
minHor x = (div x 60,mod x 60)

difHora :: Hora -> Hora -> Int
difHora x y = abs (min x - min y)

addHora :: Int -> Hora -> Hora
addHora x y = minHor( x + min y)

--------------------------------------------------------------------

data Hora = H Int Int 
      deriving (Show,Eq)

isValidHora :: Hora -> Bool
isValidHora (H h m) = (elem h [0..23]) && (elem m [0..59])

isHora :: Hora -> Hora -> Bool
isHora (H h1 m1) (H h2 m2) | h1 > h2 = True
                           | (h1 == h2) && (m1 > m2) = True
                           | otherwise = False

isMin :: Hora -> Int
isMin (H h m) = h*60 + m

isminHor :: Int -> Hora
isminHor x = H (div x 60) (mod x 60)

difHora :: Hora -> Hora -> Int
difHora (H h1 m1) (H h2 m2) = abs ((h1 * 60 + m1) - (h2 * 60 + m2))

addHora :: Int -> Hora -> Hora
addHora x (H h m) = isminHor(x + h * 60 + m)

--------------------------------------------------------------------

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo
next x = case x of
    Verde -> Amarelo
    Amarelo -> Vermelho
    Vermelho -> Verde

stop :: Semaforo -> Bool
stop x = x == Vermelho

safe :: Semaforo -> Semaforo -> Bool
safe x y = (x /= Vermelho || y /= Verde) && (y/= Vermelho || x /= Verde)

--------------------------------------------------------------------



