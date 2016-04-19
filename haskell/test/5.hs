import System.Environment
import System.IO
import System.Directory
import Data.List
import Data.Char

main = do
  args <- getArgs
  if length args == 1 then do
    let inputFile = args!!0
    fileExists <- doesFileExist inputFile 
    if fileExists then do
      content <- readFile inputFile
      if (checkParenthesis 0 content) then do
        putStrLn "Скобки расставлены правильно"
      else putStrLn "Скобки расставлены неправильно"
    else putStrLn "The file doesn't exist!"
  else putStrLn "Invalid parametrs, expected <input> file"


checkParenthesis :: (Num a, Ord a) => a -> [Char] -> Bool
checkParenthesis count [] = count == 0
checkParenthesis count (x:xs)
  | ((x == ')') && (count <= 0)) = False
  | (x == ')') = checkParenthesis (count - 1) xs
  | ((x == '(')) = checkParenthesis (count + 1) xs
  | otherwise = checkParenthesis count xs