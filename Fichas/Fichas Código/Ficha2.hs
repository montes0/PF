module Ficha2 where

--------------------------------------------------------------------

--Recebe uma lista de Doubles e calcula o somatório da soma de todos os elementos ao quadrado
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

--Recebe uma lista de inteiros e devolve a lista de todos os elementos divisíveis por 2
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0 then h : (funB t)
else (funB t)

--Recebe uma lista e devolve o último elemento numa lista
funC :: [a] -> [a]
funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []

--Recebe uma lista e vai reordená-la de trás para a frente
funD :: [a] -> [a]
funD l = g [] l
g :: [a] -> [a] -> [a]
g acc [] = acc
g acc (h:t) = g (h:acc) t

--------------------------------------------------------------------

dobros :: [Float] -> [Float]
dobros [] = []
dobros (x:xs) = (2*x : dobros xs)

numOcorre :: Char -> String -> Int
numOcorre a [] = 0
numOcorre a [x:xs] = 1 + numOcorre a xs

positivos :: [Int] -> Bool
positivos [] = True
positivos (x:xs) | (x>=0) = positivos xs
                 | otherwise = False

soPos :: [Int] -> [Int]
soPos [] = []
soPos (x:xs) | (x<0) = soPos xs
             | otherwise = x : soPos xs

somaNeg :: [Int] -> Int
somaNeg [] = []
somaNeg (x:xs) | (x<0) = x + somaNeg xs
               | otherwise = somaNeg xs

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt l | (length l>3) = tresUlt (tail l)
          | otherwise = l

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos [(x,y):xs] = y : segundos xs

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros a [] = False
nosPrimeiros a [(x,y):xs] | (a == x) = True
                          | otherwise = nosPrimeiros a xs

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos ((a,b,c):[]) = (a,b,c)
sumTriplos ((a,b,c):(d,e,f):xs) = sumTriplos ((a+d,b+e,c+f):xs)

--------------------------------------------------------------------

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (x:xs) | (isDigit x == True) = x : soDigitos xs
                 | otherwise = soDigitos xs

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (x:xs) | (isLower x) = 1 + minusculas xs
                  | otherwise = minusculas xs

nums :: String -> [Int]
nums [] = []
nums l = map digitToInt (soDigitos l)

--ou

nums2 :: String -> [Int]
nums2 xs = map (\x -> read [x] :: Int) (filter (\x -> x `elem` ['0'..'9']) xs)

--filter (\x -> x elem ['0'..'9']) xs filtra a string xs para incluir somente os caracteres que são dígitos de 0 a 9.
--map (\x -> read [x] :: Int) mapeia cada caractere filtrado para um inteiro usando a função read e a type annotation :: Int para garantir que o resultado é um inteiro.

--------------------------------------------------------------------







