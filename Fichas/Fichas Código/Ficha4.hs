module Ficha4 where

import Data.Char (isDigit, isAlpha)


digitAlpha :: String -> (String,String)
digitAlpha s = (letras, digitos)
    where
        (letras,digitos) = foldr(\c (l,d) -> if isAlpha c then (c:l, d) else (l, c:d))

nzp :: [Int] -> (Int,Int,Int)
nzp s = (negativos,zeros,positivos)
    where
        (negativos,zeros,positivos)=foldr(\c (n,z,p) -> if c<0 then (n+1,z,p) else( if c == 0 then (n,z+1,p) else (n,z,p+1))) (0,0,0) s

divMod :: Integral a => a -> a -> (a, a)
divMod x y = (div, mod)
    where
        div = fst (divMod' (abs x) (abs y) 0)
        mod = snd (divMod' (abs x) (abs y) 0)
        divMod' x y count
            | x < y = (count, x)
            | otherwise = divMod' (x - y) y (count + 1)


