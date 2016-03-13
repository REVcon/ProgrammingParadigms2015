module LW4 (
  myAnd,
  myOr,
  myAny,
  mySize,
  myIsOctDigit,
  myDif
) where


import qualified Data.Map as M
import qualified Data.Set as S

{-
Data.List.and
and :: Foldable t => t Bool -> Bool
В качестве аргумента принимет список элементов типа bool, возвращает True, если все элементы True, иначе False
Примеры:
ghci> and [True,True,True,True]  
True 
ghci> and [True,True,True,False]  
False
ghci> and $ map (>4) [5,6,7,8]  
True
-}

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs)
        | x == False = False
        | x == True = myAnd(xs)

{-
Data.List.or
or :: Foldable t => t Bool -> Bool
В качестве аргумента принимет список элементов типа bool, возвращает True, если хотя бы один элемент списка равен True, иначе False
Примеры:
ghci> or [False, False, False, True]  
True
ghci> or [False, False, False, False]  
False
ghci> or $ map (==4) [2,3,4,5,6,1]  
True
-}
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
        | x == True = True
        | x == False = myOr(xs)

{- 
Data.List.any
any :: Foldable t => (a -> Bool) -> t a -> Bool
В качестве аргументов принимает предикаит и список элементов типа bool, возвращает True, если хотя бы один элемент списка выполняет условие предиката, иначе False
Примеры:
ghci> any (==4) [2,3,5,6,1,4]  
True
ghci> any (==4) [2,3,5,6,1,4]  
True
ghci> any Data.Char.isDigit ['4','a','b']
True
-}

myAny :: (t -> Bool) -> [t] -> Bool
myAny p [] = False
myAny p (x:xs)
        | p x == True = True
        | p x == False = myAny p xs

{-
Data.Map.size
size :: Map k a -> Int
Возвращает размер переданного ассоциативного массива
Примеры:
ghci> Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]  
5
ghci> Map.size Map.empty  
0
ghci> Data.Map.size $ Data.Map.fromList[(1,2),(1,2),(1,2)]
1
-}

mySize :: M.Map k a -> Int
mySize arg = length (M.keys arg)


{-
Data.Char.isOctDigit
Data.Char.isOctDigit :: Char -> Bool
Проверяет является ли переданный символ восьмеричной цифрой
Примеры:
ghci> Data.Char.isOctDigit('1')
True
ghci> Data.Char.isOctDigit('a')
False
ghci> Data.Char.isOctDigit('8')
False
-}

myIsOctDigit :: Char -> Bool
myIsOctDigit a
        | a `elem` ['0'..'7'] = True
        | otherwise = False

{-
Data.Set.difference
difference :: Ord a => Set a -> Set a -> Set a
Принимает в качестве аргументов 2 множества, результат - множество, состоящее из элементов, которые входят в первое множество, но не входят во второе.
Примеры:
ghci> difference (fromList [1,2,3,4,5]) (fromList [7,6])
fromList [1,2,3,4,5]
ghci> difference (fromList [1,2,3,4,5]) (fromList [1,2,3])
fromList [4,5]
ghci> difference (fromList []) (fromList [])
fromList []
-}

myDif:: Ord a => S.Set a -> S.Set a -> S.Set a
myDif set1 set2
     | set1 == S.empty = S.empty
     | set2 == S.empty = set1
     | S.member (S.elemAt 0 set1) set2 = (myDif (S.deleteAt 0 set1) set2)
     | otherwise = S.insert (S.elemAt 0 set1) (myDif (S.deleteAt 0 set1) set2)

{-
Вопросы:
#1 Чему равен результат выполнения: filter (Data.Char.isOctDigit) "4a17vc9808" 
-[4,1,7,0]
-"4170"
-['4','1','7','8','0','8']
-"4179808"
#2 Чему равен результат выполнения: Data.Set.difference (Data.Set.fromList ['t','v','4','u','6','v','b','3','v','t','u']) (Data.Set.insert 't' $ Data.Set.fromList ['u','4', 'v', 'u'])
-fromList [3,6]
-fromList "63tb"
-fromList ['3','6','t','b']
-fromList "36b"
-}