-- ghci
-- :load first
module First where
--1a
harmonic 1 = 1
harmonic x = harmonic(x-1) + (1/x)

--1b
--this doesn't work because the types are conflicting, 'harmonic' only accepts
--inputs that are fractional and the length() command returns an integer
--fixed 'harmonic': use harmonic (fromIntegral(length[1,2,3,4])) as the input instead

--2
--ps :: (Double -> Int) -> Double
--State = number of games needed to win
--y = amount of games the team need to win the game
ps :: Double -> Int -> Int -> Double
ps p 0 l = 1
ps p w 0 = 0
--given the games required to win to lose and the chance of winning a game, calculates
--the probabilty of winning with a certain score c
ps p w l = p * (ps p (w-1) l) + (1 - p) * (ps p w (l-1))

--3
foo :: Int -> Int
foo x = x+3
dfoo x = foo(foo x)
nfoo x = x + 3 + 3
fooeach [] = []
fooeach (h:t) = foo h:t
nfooeach [] = []
nfooeach(h:t) = nfoo h:t
iffoo [] = []
iffoo (h:t)
    | h < 4.3 = h:t
    |otherwise = t
dd x y = 2*x + 3*y + 7

--(a)dfoo :: Int -> Int
--(b)nfoo ::Num a -> a
--(c)fooeach :: [Int] -> [Int]
--(d)nfooeach :: Num [a] -> [a]
--(e)iffoo :: (Ord a, Fractional a) => [a] -> [a]
--(f)dd: Num a => a -> a -> a
--(g) dd 4.3:: Fractional a => a -> a


--4
--(a)
myreplace :: Int -> Int -> [Int] -> [Int]
myreplace x y [] = []
myreplace x y (h:t)
    | x == h      = y : (myreplace x y t)
    | otherwise = myreplace x y t

--(b)
myapply :: [Char] -> [Char] -> [Char]
myapply l1 l2 = []
my apply (h,t1) ((x,y),t2)
    | h == x   = y: (myapply l1 l2 t2)
    | otherwise = myapply t1 t2

--(c)
myorderedlist :: [Int] -> Bool
myorderedlist [] = True
myorderedlist [x] = True
myorderedlist (x:y:xs) = x <= y && isSorted (y:xs)

--(d)
myremoveduplicates :: [a] -> [a]
myremoveduplicates [] = []
myremoveduplicates (h:t)
    |elem h == True = t: myremoveduplicates t
    |otherwise = h : myremoveduplicates t

--(e)
deln :: Int -> Char -> [Char]
deln n e [] = []
deln 0 e lst = lst
deln n e (h:t)
    |e == h   = deln (n-1) e t
    | otherwise = h: deln n e t

--(f)
--deleting n occurances of e from list lst
delna :: Int -> Char -> [[Char]]
delna n e [] = []
delna 0 e lst=lst
delna n e (h:m:t)
    |e == h   = deln (n-1) e m
                [(i,j)| i = m,
                        j = t]
    |otherwise = h: delna n e t
