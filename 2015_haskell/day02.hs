-- advent of code problem 2

module Main where

loadfile = do xs <- readFile "instring2.txt"
              return xs

--inp = "1x2x3\n2x22x3\n111x1x15"
--
--trafo []                    = [] 
--trafo (x:xs) |x=='x'        = ","++trafo xs
--             |x=='\n'       = "],["++trafo xs
--             |otherwise     = [x]++trafo xs
--
--trafo2 xs                   = "[["++trafo xs++"]]"
--trafo3 xs                   = read $ (trafo2 xs)::[[Int]]

trf [] [] []                  = []
trf cs ns []                  = (ns++[read cs::Int]):[]
trf cs ns (x:xs) |x=='x'      = trf [] (ns++[read cs::Int]) xs
                 |x=='\n'     = (ns++[read cs::Int]):trf [] [] xs
                 |otherwise   = trf (cs++[x]) ns xs

transf xs                     = trf [] [] xs

paperArea [a,b,c] |(a<=b)&&(b<=c) = 2*(a*b+b*c+a*c)+a*b
                  |(a<=b)&&(c<b)  = 2*(a*b+b*c+a*c)+a*c
                  |(c>a)          = 2*(a*b+b*c+a*c)+a*b
                  |otherwise      = 2*(a*b+b*c+a*c)+b*c

ribLen [a,b,c]    |(a<=b)&&(b<=c) = a*b*c+2*(a+b)
                  |(a<=b)&&(c<b)  = a*b*c+2*(a+c)
                  |(c>a)          = a*b*c+2*(a+b)
                  |otherwise      = a*b*c+2*(b+c)


sumArea f xss = sum [ f xs | xs <- xss]


comp1 = do acinput <- loadfile
           putStr (show $ sumArea paperArea (transf acinput))

comp2 = do acinput <- loadfile
           putStr (show $ sumArea ribLen (transf acinput))

main  = do inp <- loadfile
           putStr (show $ sumArea paperArea (transf inp))
           putStr ("\n")
           putStr (show $ sumArea ribLen (transf inp))            




    


