module Chap6 where

import Data.Char

fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * (fac (n-1))


mult m 0 = 0
mult m n = m + (m `mult`(n - 1))


reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]


insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys
                  
isort [] = []
isort (x:xs) = insert x (isort xs)

--pairs::[a] -> [(a,a)]
pairs [] = []
pairs xs = zip xs (tail xs)

sorted xs = and [ x <= y | (x,y) <- pairs xs]


let2int :: Char -> Int
let2int c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c || isUpper c = int2let ((let2int c + n) `mod` 57)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs ]

decode :: Int -> String -> String
decode n xs = [shift (- n) x | x <- xs ]

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [y | y <- xs, x == y]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a' .. 'z']]
    where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions x xs = [i | (y, i) <- zip xs [0..], x == y]                                                                       

table :: [Float]
table = [8.1,1.5,2.8,4.2,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,6.3,9.0,2.8,1.0,2.4,0.2,2.0,0.1]


crack :: String -> String
crack xs = decode factor xs
   where factor = head (positions (minimum chitable) chitable)
         chitable = [chisqr (rotate n table') table | n <- [0 .. 25]]
         table' = freqs xs

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0 .. m], y <- [0 .. n]]

square :: Int -> [(Int,Int)]
square n = [(x,y) | (x, y) <- grid n n, x /= y]

replicate' :: Int -> a -> [a]
replicate' n x = [x | i <- [1 .. n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors x = [y | y <- [1 .. x], (x `mod` y) == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]
    where perfect x = sum (factors x) - x == x

scalarproduct :: Num a => [a] -> [a] -> a
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n > 0 = n + sumdown (n - 1)

euclid :: Int -> Int -> Int
euclid x y | x == y = y
           | x > y = euclid (x - y) y
           | otherwise = euclid (y - x) x


concat' :: [[a]]  -> [a]
concat' [] = []
concat' [[]] = []
concat' (x:xs) = x ++ (concat' xs) 

at :: [a] -> Int -> a
at xs 0 = head xs
at (x:xs) n = at xs (n - 1) 

elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' y (x:xs) | x == y = True
               | otherwise = elem y xs 

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] xs = xs
merge xs ys | x <= y = x : merge xs' ys
            | otherwise = y : merge xs ys'
                where x = head xs
                      y = head ys
                      xs' = tail xs
                      ys' = tail ys 

halve :: [a] -> [[a]]
halve [] = [[], []]
halve xs = [take h xs, drop h xs]
    where h = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (head hs)) (msort (last hs))
    where hs = halve xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum(xs)

take' :: Int -> [a] -> [a]
take' 1 (x:_) = [x]
take' n (x:xs) | n > 1 = x : take' (n - 1) xs 

last' :: [a] -> a
last' [x] = x
last' (_:xs) = last' xs


toDidgits :: Integer -> [Integer]
toDidgits n | n >= 0 = if n `div` 10 == 0 then [n] else toDidgits (n `div` 10) ++ [n `mod` 10]

toDidgitsRev :: [Integer] -> Integer
toDidgitsRev ds = foldl (\x y -> y + (10 * x)) 0 ds

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs

doubleSecond :: [Integer] -> [Integer]
doubleSecond xs = [if mod i 2 == 0 then x*2 else x | (x, i) <- xs `zip` [1..]]

doubleSecond' :: [Integer] -> [Integer]
doubleSecond' = altMap id (*2)

sumDidgits :: [Integer] -> Integer
sumDidgits xs = sum [y | x <- xs, y <- toDidgits x]

isValid :: Integer -> Bool
isValid n = sumDidgits (doubleSecond' (reverse (toDidgits n))) `mod` 10 == 0
