module LW2 (
	insert,
	oddEven,
	listSumm,
	position,
	func1,
	func2	
) where

--1
oddEven :: [a] -> [a]
oddEven [] = []
oddEven [x] = [x]
oddEven xs = (reverse (take 2 xs)) ++ oddEven (drop 2 xs)

--2
insert :: [a] -> a -> Int -> [a]
insert xs x n = (take n xs) ++ (x:(drop n xs))

--3
listSumm :: Num t => [t] -> [t] -> [t]
listSumm [] [] = []
listSumm xs ys
	| length(xs) > length(ys) = (listSumm (take (length ys) xs) ys) ++ (drop (length ys) xs)
 	| length(xs) < length(ys) = (listSumm (take (length xs) ys) xs) ++ (drop (length xs) ys)
 	| otherwise  = ((head xs) + (head ys)) : (listSumm (tail xs) (tail ys))


--4
position :: Eq a => [a] -> a -> Int
position xs b
    | not (b `elem` xs) = -1
    | otherwise = head [y | y <- [if b == (xs !! x) then x else - 1|  x <- [0..((length xs) - 1)]], y > -1]
 
 
--5
func1 :: Int -> Int
func1 a
	| a < 1 = 0
	| otherwise = sum (take a [1,2..])


--6
func2 :: Int -> Int
func2 a
	| a < 1 = 0
	| otherwise = sum (take (a - 1) [1,2..])
