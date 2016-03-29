module Loop where

import Prelude hiding (Either(..)) 
import Data.Char
import System.IO
import System.Directory
import Game

data Query = Quit | NewGame Int | Play Move

data Menu = MenuNewGame | Stats | MenuQuit | Help

play :: IO()
play = greetings >> showHelp >> menu

menu :: IO ()
menu = showMenu >> askForMenuAction >>= reactOnQuery

askForMenuAction :: IO Menu
askForMenuAction = showAction >> getLine >>= maybe askAgain return . parseMenuQuery
    where askAgain = showHelp >> askForMenuAction

parseMenuQuery :: String -> Maybe Menu
parseMenuQuery query = case query of
    "new"                -> Just $ MenuNewGame
    "n"                  -> Just $ MenuNewGame
    "stats"              -> Just $ Stats
    "s"                  -> Just $ Stats
    "help"               -> Just $ Help
    "h"                  -> Just $ Help
    "quit"               -> Just $ MenuQuit 
    "q"                  -> Just $ MenuQuit 
    _                    -> Nothing


reactOnQuery :: Menu -> IO ()
reactOnQuery query = case query of
    MenuNewGame  -> setup >>= gameLoop 0
    Stats        -> getStatsFromFile >> menu 
    Help         -> showHelp >> menu 
    MenuQuit     -> quit


showMenu :: IO ()
showMenu = do
    putStrLn "Новая игра."
    putStrLn "Статистика."
    putStrLn "Справка."
    putStrLn "Выход."


showAction :: IO ()
showAction = putStrLn "Выберите действие: "

showHelp :: IO ()
showHelp = mapM_ putStrLn help
    where help = [
            "Доступные действия:",
            "new or n   -- начать новую игру",
            "stats or s -- показать статистику",
            "help or h  -- показать справку",
            "quit or q  -- выход из игры"]


getStatsFromFile :: IO ()
getStatsFromFile = do
    fileExists <- doesFileExist "stats.txt"
    if fileExists then do
        contents <- readFile "stats.txt"
        let todoTasks = lines contents
            numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [1..] todoTasks
        putStr $ unlines numberedTasks
    else putStr "Нет сохраненной статистики.\n"


showSteps :: Int -> IO ()
showSteps s = putStr "Количество ходов: " >> print s >> saveStatsToFile s


saveStatsToFile :: Int -> IO ()
saveStatsToFile s = do
    appendFile "stats.txt" ((show s) ++ "\n")

-- Основные функции

setup :: IO Game
setup = showGame initGame >>
    remindMoves>>
    putStrLn "Начнём новую игру?" >>
    putStrLn "Укажите сложность (положительное целое число): " >>
    getLine >>= maybe setup shuffle . readInt 


gameLoop :: Int -> Game -> IO ()
gameLoop steps game 
    | isGameOver game   = showResults game steps >> menu
    | otherwise         = showGame game >> askForMove >>= reactOnMove game steps


-- Запросы пользователя


reactOnMove :: Game -> Int-> Query -> IO ()
reactOnMove game steps query = case query of
    Quit        -> menu
    Play    m   -> gameLoop  (steps + 1) $ move m game

askForMove :: IO Query
askForMove = showAsk >>
    getLine >>= maybe askAgain return . parseQuery 
    where askAgain = wrongMove >> askForMove


parseQuery :: String -> Maybe Query
parseQuery x = case x of
    "up"    -> Just $ Play Up
    "u"     -> Just $ Play Up
    "down"  -> Just $ Play Down 
    "d"     -> Just $ Play Down 
    "left"  -> Just $ Play Left
    "l"     -> Just $ Play Left
    "right" -> Just $ Play Right
    "r"     -> Just $ Play Right
    "quit"  -> Just $ Quit
    "q"     -> Just $ Quit

    'n':'e':'w':' ':n   -> Just . NewGame =<< readInt n
    'n':' ':n           -> Just . NewGame =<< readInt n  
    _       -> Nothing
    

readInt :: String -> Maybe Int
readInt n 
    | all isDigit n = Just $ read n
    | otherwise     = Nothing


-- Ответы пользователю

greetings :: IO ()
greetings = putStrLn "Привет! Это игра пятнашки"


showResults :: Game -> Int -> IO ()
showResults g steps = showGame g >> putStrLn "Игра окончена." >> showSteps steps


showGame :: Game -> IO ()
showGame = putStrLn . show 


wrongMove :: IO ()
wrongMove = putStrLn "Не могу распознать ход." >> remindMoves


showAsk :: IO ()
showAsk = putStrLn "Ваш ход: "

remindMoves :: IO ()
remindMoves = mapM_ putStrLn talk
    where talk = [
            "Возможные ходы пустой клетки:",
            "   left     или l       -- налево",
            "   right    или r       -- направо",
            "   up       или u       -- вверх",
            "   down     или d       -- вниз",
            "Другие действия:",
            "   new int  или n int -- начать новую игру, int - целое число,", 
                                      "указывающее на сложность",
            "   quit     или q      -- выход из игры"]

quit :: IO ()
quit = putStrLn "До встречи." >> return ()
