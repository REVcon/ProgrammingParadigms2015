import System.Environment
import System.IO
import System.Directory
import Data.List
import Data.Char

main = do
  args <- getArgs
  if length args == 2 then do
    let inputFile = args!!0
    let outputFile = args!!1
    fileExists <- doesFileExist inputFile 
    if fileExists then do
      putStrLn "Enter the symbol for replace: "
      symbol <- getChar
      content <- readFile inputFile
      writeFile outputFile (replacePunctuationChars content symbol)
    else putStrLn "The file doesn't exist!"
  else putStrLn "Invalid parametrs, expected <input> <output> files"


replacePunctuationChars  :: String -> Char -> String
replacePunctuationChars  [] c = []
replacePunctuationChars  (x:xs) c
  | isPunctuation x = c : (replacePunctuationChars xs c)
  | otherwise = x : (replacePunctuationChars xs c)