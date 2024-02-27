module KociembaSolver where

import System.Process -- run ":set -package process" before loading SolveModule.hs

import Cube
import Face
import Data.Char
import Data.Maybe

-- Converts the cube to a string representation that can be understood by kociemba.py
convertToKociembaString :: Cube -> String
convertToKociembaString (Cube uFace dFace rFace lFace fFace bFace) =
    replaceColours $
    removeSpacesAndLower $ unwords [showRowWithoutANSICodes face idx | face <- [uFace, rFace, fFace, dFace, lFace, bFace], idx <- [0, 1, 2]]

    where
      removeSpacesAndLower :: String -> String
      removeSpacesAndLower = map toLower . filter (/= ' ')

      replaceColours :: String -> String
      replaceColours = map substitute
        where
        substitute 'r' = 'U'
        substitute 'w' = 'F'
        substitute 'b' = 'L'
        substitute 'g' = 'R'
        substitute 'y' = 'B'
        substitute 'o' = 'D'

-- Maps each move to a function
moveFunctions :: [(String, Cube -> Cube)]
moveFunctions =
  [ ("U", u), ("U'", u'), ("U2", u2), ("U2'", u2)
  , ("R", r), ("R'", r'), ("R2", r2), ("R2'", r2)
  , ("F", f), ("F'", f'), ("F2", f2), ("F2'", f2)
  , ("L", l), ("L'", l'), ("L2", l2), ("L2'", l2)
  , ("B", b), ("B'", b'), ("B2", b2), ("B2'", b2)
  , ("D", d), ("D'", d'), ("D2", d2), ("D2'", d2)
  ]

-- List of moves
moveList :: [String]
moveList = map fst moveFunctions

-- Takes in a cube and the string representation of the move and returns the new cube after performing the move
doMove :: Cube -> String -> Cube
doMove cube move = if move `elem` moveList then fromJust (lookup move moveFunctions) cube else error "Invalid move"

-- Solves the cube using the Kociemba's algorithm
solveCube :: Cube -> IO (Cube, [String])
solveCube cube = do
  output <- readProcess "python3" ["-c", "import kociemba; print(kociemba.solve('" ++ convertToKociembaString cube ++ "'))"] ""
  let solution = words output
  return (foldl doMove cube solution, solution)