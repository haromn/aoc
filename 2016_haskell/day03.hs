-- advent of code 2016 problem 3

module Main3a where

--elem x xs = any (==x) xs
isSpace x = elem x " \t"
isDigit x = elem x "0123456789"


trafo cs ns []                = (ns++[read cs::Int]):[]

trafo cs ns (x:xs) |(isSpace x && cs==[]) = trafo [] ns xs
                   |isSpace x             = trafo [] (ns++[read cs::Int]) xs
                   |(x=='\n' && cs==[])   = ns:trafo [] [] xs
                   |x=='\n'               = (ns++[read cs::Int]):trafo [] [] xs
                   |isDigit x             = trafo (cs++[x]) ns xs

trianglist xs = trafo [] [] xs

sumside [a,b,c] = [(a+b,c),(a+c,b),(b+c,a)]

isValid p = (fst p) > (snd p)

isValidTriangs xss  = [and (map isValid xs)| xs <- xss]

numValidTriangs xss = length $ filter (==True) (isValidTriangs xss)

comp1 = do inp <- readFile "instring3.txt"
           print (numValidTriangs $ map sumside $ trianglist $ inp)


rezip [] [] []              = []
rezip (a:as) (b:bs) (c:cs)  = [a,b,c]:rezip as bs cs

retrianglist []             = []
retrianglist (as:bs:cs:xss) = (rezip as bs cs)++retrianglist xss

newtrianglist xs = retrianglist $ trianglist xs

comp2 = do inp <- readFile "instring3.txt"
           print (numValidTriangs $ map sumside $ newtrianglist $ inp)
