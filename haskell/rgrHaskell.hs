import System.IO
import Control.Monad

fileName1 = "1.txt"

fileName2 = "2.txt"

go = do
    createFile fileName1
    createFile fileName2
    makeMove 3 2 0
    printResult

makeMove :: Integer -> Integer -> Integer -> IO ()
makeMove x y steps = do
    if (not(opponentWinInAnyMove x y)) then do
        let newsteps = steps + 1
        when (not(willOpponentWin (x+3) y)) $ makeMove (x+3) y newsteps
        when (not(willOpponentWin x (y+2))) $ makeMove x (y+2) newsteps
        when (not(willOpponentWin x (y + 4))) $ makeMove x (y+4) newsteps
    else do
        defineWinner steps

willOpponentWin :: (Num a, Ord a) => a -> a -> Bool
willOpponentWin x y = (x + y + 3 > 12) || (x + y + 2 > 12) || (x + y + 4 > 12)

opponentWinInAnyMove :: (Num a, Ord a) => a -> a -> Bool
opponentWinInAnyMove x y = (willOpponentWin x (y + 4)) && (willOpponentWin (x+3) y) && (willOpponentWin x (y + 2))

defineWinner :: Integer -> IO ()
defineWinner steps = do    
    let r = steps `mod` 2
    if (r == 0) then checkPlayer2
    else checkPlayer1


checkPlayer1 = do    
    handle <- openFile fileName1 ReadMode
    contents <- hGetLine handle
    hClose handle 
    let x = (read contents :: Integer) + 1
    writeFile fileName1 (show x)


checkPlayer2 = do    
    handle <- openFile fileName2 ReadMode
    contents <- hGetLine handle
    hClose handle 
    let x = (read contents :: Integer) + 1
    writeFile fileName2 (show x)


printResult = do    
    handle <- openFile fileName1 ReadMode
    contents <- hGetLine handle
    hClose handle
    let player1 = (read contents :: Integer)    
    handle <- openFile fileName2 ReadMode
    contents <- hGetLine handle
    hClose handle
    let player2 = (read contents :: Integer)
    if (player1 > player2)
        then print "Player 1 will be winner in logical game"
        else print "Player 2 will be winner in logical game"

createFile filename = do
    writeFile filename "0" 
    


