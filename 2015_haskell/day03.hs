-- advent of code problem 3

module Main where

testInp = "^>v<<<"

type Pos = (Int,Int)

step :: Pos -> Char -> Pos
step p c = case c of
             '^' -> (fst p,(snd p)+1) 
             'v' -> (fst p,(snd p)-1) 
             '>' -> ((fst p)+1,snd p) 
             '<' -> ((fst p)-1,snd p)

walk :: Pos -> [Char] -> [Pos]
walk p []     = [p]
walk p (c:cs) = p:walk (step p c) cs

zwalk cs      = walk (0,0) cs

oddelems []      = []
oddelems (c:cs)  = c:evenelems cs
evenelems []     = []
evenelems (_:cs) = oddelems cs

pathA ps = oddelems ps
pathB ps = evenelems ps

removeDupl []   = []
removeDupl (p:ps) = p:removeDupl (filter (/=p) ps)

res = length $ removeDupl $ zwalk testInp 

comp1 = do inp <- readFile "instring3.txt"
           print (length $ removeDupl $ zwalk inp)

comp2 = do inp <- readFile "instring3.txt"
           print (length $ removeDupl $ zwalk (pathA inp) ++ zwalk (pathB inp)) 


