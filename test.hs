double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]
average ns = sum ns `div` length ns

mysum [] = 0
mysum (x:xs) = x + sum xs

fac 0 = 1
fac n = n * fac (n - 1)

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                   smaller = [a | a <- xs, a <= x]
                   larger  = [a | a <- xs, a > x ]

seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)

n = a `div` length xs
        where
          a = 10
          xs = [1,2,3,4,5]

myeven :: Integral a => a -> Bool
myeven n = n `mod` 2 == 0

mysignum n | n > 0  = 1
           | n == 0 = 0
           | otherwise = -1


halve xs = (take h xs, drop h xs)
            where h = length xs `div` 2

third xs = head (drop 2 xs) 
thirdi xs = (xs !! 2)
thirdp (x:y:z:rest) = 

safetail [] = []
safetail (_:xs) = xs

safetail2 xs = case xs of
  [] -> []
  (_:rest) -> rest

mor x y | x /= y = True
        | x = False
        | otherwise = False

mult = \x -> (\y -> (\z -> x * y * z))

luhnDouble x = if n > 9 then n - 9 else n
  where n = 2*x

-- luhn a b c d = total `mod` 10 == 0
--  where total = d + (luhnDouble c) + (luhnDouble d) + (luhnDouble a)