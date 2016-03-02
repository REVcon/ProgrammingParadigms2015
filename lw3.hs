module LW3 (
	listnums,
	secondlastlist,
	myunion,
	mysubst,
	getElements
) where

--1
listnums :: Int -> [Int]
listnums n  
	| n < 1 = [] 
	| otherwise =  n : listnums (n-1)

--2
secondlastlist :: [[t]] -> [t]
secondlastlist [] = []
secondlastlist [[]] = []
secondlastlist (xs:xss) = last xs : secondlastlist xss

--3
myunion ::Eq a => [a] -> [a] -> [a]
myunion [] [] = []
myunion [] (y:ys)
	| not (y `elem` ys) = y : myunion [] ys
	| otherwise = myunion [] ys
myunion (x:xs) ys
	| not ((x `elem` xs) || (x `elem` ys )) = x : myunion xs ys
	| otherwise = myunion xs ys


--4
mysubst :: Eq a => [a] -> [a] -> [a]
mysubst [] [] = []
mysubst xs [] = xs
mysubst [] ys = []
mysubst (x:xs) ys
	| x `elem` ys = mysubst xs ys
	| otherwise = x : mysubst xs ys 

--6
getElements :: [[b]] -> Int -> [b]
getElements xss n = map (\xs -> xs !! n ) [xs | xs <- xss, length xs > n]