add       :: Int -> Int -> Int -> Int
add x y q = x + y + q   

twice f x = f x
 
ss x  | x > 4     = [x]
      | otherwise = [x, x, x]

mult 0 _ = 0
mult 1 x = x
mult x y = mult (x - 1) y + y

mapp _ []     = []
mapp f (x:xs) = f x :  f `mapp` xs

filterr _ []                 = []
filterr f (x:xs) | f x       = [x] ++ filterr f xs
                 | otherwise = filterr f xs

flat xss = [x | xs <- xss, x <- xs]

replicatee 0 _ = []
replicatee x a = a : replicate (x - 1) a

(@@) :: [a] -> Int -> a
(x:xs) @@ 0 = x
(x:xs) @@ n = xs @@ (n - 1)

include _ [] = False
include y (x:xs) | x == y      = True
                 | otherwise  = include y xs
