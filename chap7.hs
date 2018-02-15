module Chap7 where

import Data.Char
import Data.List

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse' :: [a] -> [a]
reverse' xs = foldr snoc [] xs

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

parity :: [Bit] -> [Bit]
parity bits | (odd . sum) bits = 1 : bits
            | otherwise = 0 : bits

encode :: String -> [Bit]
encode = concat . map (parity . make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = (checkParity . (take 9)) bits : chop8 (drop 9 bits)

checkParity :: [Bit] -> [Bit]
checkParity bits | ((odd . sum . tail) bits) == (if head bits == 1 then True else False) = tail bits 
                 | otherwise = error "checksum failed"

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: ([Bit] -> [Bit]) -> String -> String
transmit channel = decode . channel . encode

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result votes = sort [((count c votes), c)  | c <- rmdups votes]

winner :: Ord a => [a] -> a
winner = snd . last . result

ballots :: [[String]]
ballots = [["Red","Green"],
           ["Blue"],
           ["Green", "Red", "Blue"],
           ["Blue", "Green", "Red"],
           ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                [a]    -> a
                (x:xs) -> winner' (elim x bs) 

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldl (\z x -> z && p x) True

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (\x z -> if (p x) || z then True else (p x) || z) False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f = foldr (\x z -> if f x then x:z else [] ) []  

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) | f x = dropWhile' f xs
                    | otherwise = x:xs

map' f = foldr (\x xs -> (f x):xs) [] 
filter' f = foldr (\x xs -> if f x then x:xs else xs) []

dec2Int :: [Int] -> Int
dec2Int = foldl (\z x -> z * 10 + x) 0

curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x,y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \x -> f (fst x) (snd x) 

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)


chop8' :: [Bit] -> [[Bit]]
chop8' = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f 


altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs