-- advent of code problem 1

module Main where

loadfile = do xs <- readFile "instring.txt"
              return xs

enum1 xs = zip [1..n] xs
		where n = length xs

aggregate :: Int -> [Int] -> [Int]
aggregate n [] = []
aggregate n (x:xs) = m:(aggregate m xs)
                      where m = n+x

aggr xs = aggregate 0 xs

rewrite [] = []
rewrite (x:xs) = n:rewrite xs
                   where n |x=='(' = 1
                           |x==')' = -1

res xs = enum1 (aggr (rewrite xs))

findfirst a (x:xs) |a==snd x  = fst x
                   |otherwise = findfirst a xs

findres xs = findfirst (-1) (res xs)

comp1 = do acinput <- loadfile
           putStr (show $ head $ reverse $ aggr $ rewrite $ acinput)

comp2 = do acinput <- loadfile
           putStr (show $ findres acinput)



    


