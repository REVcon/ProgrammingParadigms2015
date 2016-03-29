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
      putStrLn "Enter string for search: "
      symbol <- getLine
      content <- readFile inputFile
      let todoTasks = lines content
      let res = filter (symbol `isInfixOf`) todoTasks
      writeFile outputFile (unlines res)
    else putStrLn "The file doesn't exist!"
  else putStrLn "Invalid parametrs, expected <input> <output> files"
