-- advent of code 2016 problem 1

module Main1 where

c1 :: (a,b,c) -> a
c1 (x,_,_) = x

c2 :: (a,b,c) -> b
c2 (_,x,_) = x

c3 :: (a,b,c) -> c
c3 (_,_,x) = x

trafo [] []                = []
trafo ns []                = (read ns::Int):[]
trafo ns (x:xs) |x=='R'    = 1:trafo [] xs
                |x=='L'    = (-1):trafo [] xs
                |x==' '    = trafo [] xs
                |x==','    = (read ns::Int):trafo [] xs
                |otherwise = trafo (ns++[x]) xs

odds []      = []
odds (x:xs)  = x:evens xs                    
evens []     = []
evens (_:xs) = odds xs

orderlist xs = zip (odds trafolist) (evens trafolist) 
               where trafolist = trafo [] xs

-- (x,y,dir) dir = n=0,e=1,s=2,w=3
type Dir = Int
type Pos = (Int,Int,Dir)

step :: Pos -> (Int,Int) -> Pos
step p o = case (mod ((c3 p) + (fst o)) 4) of
              0 -> ( c1 p         ,(c2 p) + (snd o),0)
              1 -> ((c1 p)+(snd o), c2 p           ,1)             
              2 -> ( c1 p         ,(c2 p) - (snd o),2)
              3 -> ((c1 p)-(snd o), c2 p           ,3)             

walk :: Pos -> [(Int,Int)] -> Pos
walk p []     = p
walk p (o:os) = walk (step p o) os

walkz = walk (0,0,0) 

l1norm :: Pos -> Int
l1norm p = (abs (c1 p)) + (abs (c2 p))

distwalked os = l1norm $ walkz $ os

comp1 = do inp <- readFile "instring1.txt"
           print (distwalked $ orderlist $ inp)

straight dir 0 = []
straight dir n = dir:straight dir (n-1) 

trafo2 :: [Char] -> Dir -> [Char] -> [Int]
trafo2 [] dir []                = []
trafo2 ns dir []                = (straight dir (read ns::Int))
trafo2 ns dir (x:xs) |x=='R'    = trafo2 [] (mod (dir+1) 4) xs
                     |x=='L'    = trafo2 [] (mod (dir-1) 4) xs
                     |x==' '    = trafo2 [] dir xs
                     |x==','    = (straight dir (read ns::Int))++trafo2 [] dir xs
                     |otherwise = trafo2 (ns++[x]) dir xs

steplist xs = trafo2 [] 0 xs

type Vec = (Int,Int)

step2 :: Vec -> Int -> Vec
step2 p n = case n of
               0 -> (fst p,(snd p)+1) 
               2 -> (fst p,(snd p)-1) 
               1 -> ((fst p)+1,snd p) 
               3 -> ((fst p)-1,snd p)

walkpath :: Vec -> [Int] -> [Vec]
walkpath p []     = [p]
walkpath p (c:cs) = p:walkpath (step2 p c) cs

walkpathZ cs      = walkpath (0,0) cs

l1norm2 :: Vec -> Int
l1norm2 p = (abs (fst p)) + (abs (snd p))

firstCrossP past (p:future) = case find of
                                 [] -> firstCrossP (past++[p]) future                             
                                 cs -> head cs
                              where find = filter (==p) past

firstCross :: Eq a => [a] -> a
firstCross = firstCrossP []

comp2 = do inp <- readFile "instring1.txt"
           print (l1norm2 $ firstCross $ walkpathZ $ steplist $ inp)
           
