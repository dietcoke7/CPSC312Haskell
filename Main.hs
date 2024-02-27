import KociembaSolver
import Cube

import System.IO
import System.Random -- run ":set -package random" before loading Main.hs

import FileIO

-- Defines the entry point of the program
main :: IO ()
main = runMenu initialCube
  where
    initialCube = defaultCube

-- Defines the initial running of the menu
runMenu :: Cube -> IO ()
runMenu cube = do
    putStrLn "Hi There! Welcome to the Rubik's Cube App \n"
    displayMenu
    loop cube

-- Displays the menu
displayMenu :: IO ()
displayMenu = do
    putStrLn "Chose from the following options:"
    putStrLn "\tmenu -> view this menu again"
    putStrLn "\tview -> view the cube"
    putStrLn "\tmoves -> view all possible cube moves and also moves performed"
    putStrLn "\tdomove <some move(s) separated by spaces> -> move the cube (eg: do move R' D F2 -> does the moves R', D and F2 on the cube)"
    putStrLn "\tshuffle n -> shuffle the cube using n moves (eg: shuffle 7 -> shuffles using 7 moves)"
    putStrLn "\tsolve -> solve the cube using the Kociemba method"
    putStrLn "\tload -> load a cube from the file \"cube.txt\""
    putStrLn "\tsave -> save the cube to the file \"cube.txt\""
    putStrLn "\tquit -> quit the app"
    putStrLn "Note: To chose multiple options at once, enter their keywords separated by ;"

-- The main loop
loop :: Cube -> IO ()
loop cube = do
    putStr ">> "
    hFlush stdout
    commands <- getLine
    let commandsArray = map trim (splitBySemicolon commands)
    handleCommands cube commandsArray

-- Deals with all the commands that the user enters
handleCommands :: Cube -> [String] -> IO ()
handleCommands cube [] = loop cube
handleCommands cube (command:commands) = do
    case head (words command) of   -- extracts the first word
        "menu" -> do
            displayMenu
            handleCommands cube commands
        "view" ->  do
            print cube
            handleCommands cube commands
        "moves" -> do
            print moveList
            handleCommands cube commands
        "domove" -> do
            let moves = tail (words command)
            let newCube = foldl doMove cube moves
            handleCommands newCube commands
        "shuffle" -> do
            let n = stringToInt (words command !! 1)
            (shuffledCube, shuffleMoves) <- doShuffle cube n
            putStrLn ("Shuffled the cube using the following " ++ show n ++ " moves:")
            print shuffleMoves
            handleCommands shuffledCube commands
        "solve" -> do
            (solvedCube, solution) <- solveCube cube
            putStr "Solved the cube with " >> print (length solution) >> putStr " moves! Here is the solution:" >> print solution

            handleCommands solvedCube commands
        "load" -> do
            loadedCube <- loadCube
            handleCommands loadedCube commands
        "save" -> do
            saveCube cube
            handleCommands cube commands
        "quit" -> do
            putStrLn "Quitting the Rubik's Cube App. Thanks for your time! :)"
        _ -> putStrLn "invalid choice" >> loop cube

-- Takes a cube and shuffles it using n moves
doShuffle :: Cube -> Int -> IO (Cube, [String])
doShuffle cube n = shuffleHelper cube n []
  where
    shuffleHelper :: Cube -> Int -> [String] -> IO (Cube, [String])
    shuffleHelper currentCube 0 moves = return (currentCube, moves)
    shuffleHelper currentCube remainingMoves moves = do
        index <- randomRIO (0, length moveList - 1)
        let randomMove = moveList !! index
        let newCube = doMove currentCube randomMove
        shuffleHelper newCube (remainingMoves - 1) (randomMove : moves)

-- Utility functions -- 

stringToInt :: String -> Int
stringToInt s = read s :: Int

isSpace :: Char -> Bool
isSpace c = c == ' '

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- Source: https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

splitBySemicolon :: String -> [String]
splitBySemicolon = wordsWhen (==';')