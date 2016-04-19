main = do
     putStrLn "Enter start value: "
     startVal <- readNumber
     putStrLn "Enter count of elements: "
     count <- readNumber
     putStrLn "Enter ratio: "
     ratio <- readNumber
     print (startVal : (makeList (startVal + 1) (count - 1) ratio))

readNumber :: IO Integer
readNumber = readLn

makeList :: Integer -> Integer -> Integer  -> [Integer]
makeList startVal 0 ratio = []
makeList startVal count 0 = [startVal]
makeList startVal count ratio
   | startVal `mod` ratio /= 0 = makeList (startVal + 1) count ratio
   | otherwise = startVal : makeList (startVal + 1) (count - 1) ratio