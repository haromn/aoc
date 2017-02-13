-- advent of code 2016 problem 2

module Main2b where

trafo :: [Char] -> [Char] -> [[Char]]
trafo [] []                = []
trafo cs []                = cs:[]
trafo cs (x:xs) |x=='\n'   = cs:trafo [] xs
                |otherwise = trafo (cs++[x]) xs

steplist :: [Char] -> [[Char]]
steplist = trafo []

type Pos = Char
type Dir = [Char]

--field
up = "5XA62XDB731XC84X9X"
down = "5X26AX137BDX48CX9X"
right = "1X234X56789XABCXDX"
left = "1X432X98765XCBAXDX"           

sndEq c item = (snd item) == c 

findGet l c = if hit=='X' then c else hit 
              where hit = l !! (k+1)
                    k = fst $ head (filter (sndEq c) nl)
                    nl = zip [0..n] l
                    n = (length l)-1 

move :: Pos -> Dir -> Pos 
move c dir = findGet dir c 

step1 :: Pos -> Char -> Pos 
step1 p c = case c of
               'U' -> move p up
               'D' -> move p down
               'L' -> move p left
               'R' -> move p right

walk :: Pos -> [Char] -> Pos
walk p []     = p
walk p (x:xs) = walk (step1 p x) xs

walkZ :: [Char] -> Pos
walkZ = walk '5'

solve :: [[Char]] -> [Pos]
solve []       = []
solve (xs:xss) = (walkZ xs):solve xss

solve2 :: Pos -> [[Char]] -> [Pos]
solve2 p []       = []
solve2 p (xs:xss) = q:solve2 q xss
                    where q = walk p xs

testinp = "ULL\nRRDDD\nLURDL\nUUUUD"

comp2 = do inp <- readFile "instring2.txt"
           print (solve $ steplist $ inp)
           print (solve2 '5' $ steplist $ inp)


           
