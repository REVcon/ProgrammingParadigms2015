import System.Environment
import System.IO
import System.Directory
import Data.List
import Data.Char


dispatch :: [([Char], [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)
            , ("copy", copy)  
            ]  

main = do
  args <- getArgs 
  if (length args == 2) then do
    let inputFile = args !! 0
    fileExists <- doesFileExist inputFile 
    if fileExists then do
      putStrLn "Enter command: "
      command <- getLine
      let (Just action) = lookup command dispatch  
      action args 
    else putStrLn "The file doesn't exist!"
  else putStrLn "Invalid parametrs, expected <input> <output> files"


add :: [String] -> IO ()
add args = do
  let fileName = args !! 0
  putStrLn "Enter some string to add: "
  todoItem <- getLine
  appendFile fileName (todoItem ++ "\n")


view :: [String]-> IO ()
view args = do
  let fileName = args !! 0
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStr $ unlines numberedTasks


remove :: [String] -> IO ()
remove args = do
  let fileName = args !! 0
  putStrLn "Enter number of string to delete: "
  numberString <- getLine
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberString
      todoTasks = lines contents
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName

copy :: [String]-> IO ()
copy args = do
  let inputName = args !! 0
  let outputName = args !! 1
  content <- readFile inputName
  putStrLn "Enter type of copying('withoutEmptyLines' to copy without empty lines or 'onlyDigits' to copy only digit characters): "
  filter <- getLine 
  if (filter == "withoutEmptyLines") then (writeFile outputName (deleteExtraLines content))
  else if (filter == "onlyDigits") then (writeFile outputName (onlyDigits content))
  else putStrLn "Unknown filter!"


deleteExtraLines :: String -> String
deleteExtraLines arg = unlines (filter (/= "") (lines arg))


onlyDigits :: String -> String
onlyDigits [] = []
onlyDigits (x:xs)
  | isDigit x || isControl x = x : (onlyDigits xs)
  | otherwise = (onlyDigits xs)