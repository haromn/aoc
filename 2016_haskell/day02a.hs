-- advent of code 2016 problem 2

module Main1 where

trafo :: [Char] -> [Char] -> [[Char]]
trafo [] []                = []
trafo cs []                = cs:[]
trafo cs (x:xs) |x=='\n'   = cs:trafo [] xs
                |otherwise = trafo (cs++[x]) xs

steplist :: [Char] -> [[Char]]
steplist = trafo []

type Pos = Int

step1 :: Pos -> Char -> Pos 

step1 p c = case c of
               'U' -> if p<=3 then p else (p-3)
               'D' -> if p>=7 then p else (p+3)
               'L' -> if (p==1 || p==4 || p==7) then p else (p-1)
               'R' -> if (p==3 || p==6 || p==9) then p else (p+1)

walk :: Pos -> [Char] -> Pos
walk p []     = p
walk p (x:xs) = walk (step1 p x) xs

walkZ :: [Char] -> Pos
walkZ = walk 5

solve :: Pos -> [[Char]] -> [Int]
solve p []       = []
solve p (xs:xss) = q:solve q xss
                   where q = walk p xs

testinp = "ULL\nRRDDD\nLURDL\nUUUUD"

comp1 = do inp <- readFile "instring2.txt"
           print (solve 5 $ steplist $ inp)


           
